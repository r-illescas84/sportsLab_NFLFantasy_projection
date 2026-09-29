"""
Acceso a datos para el pipeline de WR semanal, sobre nflreadpy.

Cada funcion consulta la fuente directamente -- no se guardan copias propias
en CSV. El cache en disco de nflreadpy (NFLREADPY_CACHE_DIR, configurado en
docker-compose.yml) es lo que evita volver a descargar lo mismo.

depth_charts se maneja con 2 funciones separadas, no 1 unificada: nflreadpy
regresa un esquema distinto para temporadas <=2024 que para 2025 en
adelante, sin columna de temporada/semana compartida (ver docs/HALLAZGOS.md).
Unificarlas de verdad requeriria mapear la fecha de cada snapshot (`dt`) a un
numero de semana historico -- no se construye esa pieza hasta que la Fase 4
decida, con evidencia, si depth chart es una feature que vale la pena.
"""
import pandas as pd
import nflreadpy as nfl


def cargar_jugadores():
    """Identidad de jugador. Reemplaza al archivo de ADP para esto -- ahi se
    encontraron colisiones de gsis_id entre jugadores distintos; aqui, sobre
    24,834 jugadores historicos, no hay ninguna (ver DATASHEET.md).

    birth_date llega como texto (string), no como fecha, pese a tener forma
    de fecha ISO ('1987-01-25') -- se convierte aqui explicitamente. Sin este
    paso, cualquier calculo de edad o comparacion de fechas fallaria o daria
    un resultado incorrecto sobre texto en vez de sobre fechas reales."""
    df = nfl.load_players().to_pandas()
    df["birth_date"] = pd.to_datetime(df["birth_date"], errors="coerce")
    return df


def cargar_calendario(seasons):
    """Calendario de partidos, solo temporada regular. load_schedules()
    incluye playoffs por default (game_type WC/DIV/CON/SB/SBBYE) -- se
    filtran aqui para que el resto del pipeline no los reciba sin querer.

    gameday llega como texto, mismo caso que birth_date en cargar_jugadores()
    -- se convierte aqui a fecha real."""
    sched = nfl.load_schedules(seasons)
    sched = sched.filter(sched["game_type"] == "REG")
    df = sched.to_pandas()
    df["gameday"] = pd.to_datetime(df["gameday"], errors="coerce")
    return df


def cargar_stats_semanales(seasons):
    """Estadisticas por jugador y semana, solo temporada regular. Igual que
    load_schedules(), load_player_stats() mezcla playoffs por default
    (columna season_type: REG/POST) -- se filtran aqui. Sirve igual para
    historico que para la temporada en curso: verificado que, a diferencia de
    import_weekly_data() (nfl_data_py), no tiene rezago de publicacion (ver
    DATASHEET.md, seccion Vigencia)."""
    ws = nfl.load_player_stats(seasons, summary_level="week")
    ws = ws.filter(ws["season_type"] == "REG")
    return ws.to_pandas()


def cargar_jugadas(seasons):
    """Detalle jugada por jugada, solo temporada regular (mismo patron que
    cargar_calendario/cargar_stats_semanales: load_pbp() tambien mezcla
    playoffs via season_type). Se usa para calcular target_share y otros
    agregados que no vienen directo en cargar_stats_semanales()."""
    pbp = nfl.load_pbp(seasons)
    pbp = pbp.filter(pbp["season_type"] == "REG")
    return pbp.to_pandas()


def cargar_depth_charts_historico(seasons, posicion="WR"):
    """Depth chart por temporada/semana, solo para temporadas <=2024 (esquema
    viejo de nflreadpy). Usa esto para features de entrenamiento historico.

    OJO: filtrar solo por la columna `position` NO basta -- un jugador puede
    aparecer con position="WR" en una fila de formation="Special Teams" (ej.
    como regresador de despejes), que no es su rol de ataque. Se filtra por
    formation=="Offense" y depth_position (el rol dentro de esa formacion).

    `depth_team` viene como texto ("1", "2", "3") y no es un ranking estricto
    sin empates -- varios jugadores pueden compartir depth_team="1" el mismo
    equipo/semana (ej. en paquetes de 3 WR titulares). Se regresa tal cual,
    como entero, para que quien lo use decida como desempatar si hace falta.

    Mismo patron que las demas fuentes: tambien mezcla playoffs -- 234 filas
    con game_type="SBBYE" (la semana de descanso antes del Super Bowl) traen
    `week` nulo (estructural: esa semana no tiene numero). Se filtra aqui a
    game_type=="REG", igual que en cargar_calendario/cargar_stats_semanales.

    Ademas: game_type=="REG" por si solo no basta -- aparece una "semana 19"
    etiquetada como REG incluso en equipos que no llegaron a playoffs (ej.
    Atlanta 2024, que termino 8-9), que no puede ser un partido real. Parece
    un snapshot extra de fin de temporada mal etiquetado. Se acota explicito
    a semana <= 18 (el maximo real de temporada regular desde 2021)."""
    d = nfl.load_depth_charts(seasons)
    d = d.filter(
        (d["formation"] == "Offense")
        & (d["depth_position"] == posicion)
        & (d["game_type"] == "REG")
        & (d["week"] <= 18)
    )
    df = d.to_pandas()
    df["week"] = df["week"].astype(int)
    df["depth_team"] = df["depth_team"].astype(int)
    return df[["season", "week", "club_code", "gsis_id", "full_name", "depth_team"]]


def cargar_depth_charts_actual(seasons, posicion="WR"):
    """Depth chart para 2025 en adelante -- incluida la temporada en curso
    (esquema nuevo de nflreadpy). Usa esto para la prediccion de la semana en
    curso, no para entrenamiento historico (ver cargar_depth_charts_historico).

    OJO: `pos_grp` NO es la posicion del jugador -- son paquetes de formacion
    ("3WR 1TE", "Base 4-3 D"). La posicion real vive en `pos_abb`/`pos_name`
    ("WR" / "Wide Receiver"). Con ese filtro, `pos_rank` si es un ranking
    limpio, sin empates (1, 2, 3, ... dentro del equipo).

    No hay columna de semana -- `dt` es la fecha/hora del scrape (puede haber
    varias por dia). Para "la alineacion de hoy", filtrar por el `dt` mas
    reciente por equipo sobre el resultado de esta funcion."""
    d = nfl.load_depth_charts(seasons)
    d = d.filter(d["pos_abb"] == posicion)
    df = d.to_pandas()
    df["dt"] = pd.to_datetime(df["dt"])
    return df[["dt", "team", "gsis_id", "player_name", "pos_rank"]]
