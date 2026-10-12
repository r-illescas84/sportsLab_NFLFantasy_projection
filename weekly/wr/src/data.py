"""
Acceso a datos para el pipeline de WR semanal, sobre nflreadpy.

Cada funcion consulta la fuente directamente -- no se guardan copias propias
en CSV. El cache en disco de nflreadpy (NFLREADPY_CACHE_DIR, configurado en
docker-compose.yml) es lo que evita volver a descargar lo mismo.

depth_charts: nflreadpy regresa un esquema distinto para temporadas <=2024
que para 2025 en adelante, sin columna de temporada/semana compartida (ver
docs/HALLAZGOS.md). cargar_depth_charts_historico()/cargar_depth_charts_actual()
son las 2 piezas de bajo nivel, una por esquema; cargar_depth_charts_unificado()
las combina en un solo DataFrame (season/week/team/player_id/depth_team) que
cubre 2016 en adelante -- es la que usa features.construir_tabla_modelado().
"""
import pandas as pd
import nflreadpy as nfl

CODIGOS_EQUIPO_ACTUALES = {"OAK": "LV", "SD": "LAC"}


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

    Ademas: game_type=="REG" por si solo no basta -- aparece una semana de
    mas etiquetada como REG incluso en equipos que no llegaron a playoffs (ej.
    "semana 19" de Atlanta 2024, que termino 8-9; "semana 18" en 2016-2020,
    temporadas de 17 semanas), que no puede ser un partido real. Parece un
    snapshot extra de fin de temporada mal etiquetado. Se acota a la ultima
    semana real de temporada regular: 17 hasta 2020 y 18 desde 2021.

    club_code usa el codigo de la epoca (OAK hasta 2019, SD en 2016), mientras
    que load_player_stats usa el de la franquicia actual (LV, LAC) en todas las
    temporadas. Se traduce al actual para que el cruce por equipo no deje sin
    alineacion a esos equipos (ver docs/HALLAZGOS.md).

    Un jugador puede quedar listado 2 veces en la misma semana dentro de la
    MISMA formacion/posicion, con depth_team distinto (confirmado: 357 de
    27,928 combinaciones jugador-equipo-semana en 2016-2024, ej. Braxton
    Miller, HOU, 2016 semana 1, depth_team=2 y 3 a la vez -- ver
    docs/HALLAZGOS.md). Se resuelve quedandose con el mejor rango
    (depth_team minimo) por jugador-semana antes de regresar el resultado,
    para que cualquier merge por (season, week, team, gsis_id) no duplique
    filas en silencio."""
    d = nfl.load_depth_charts(seasons)
    d = d.filter(
        (d["formation"] == "Offense")
        & (d["depth_position"] == posicion)
        & (d["game_type"] == "REG")
        & ((d["week"] <= 17) | ((d["season"] >= 2021) & (d["week"] == 18)))
    )
    df = d.to_pandas()
    df["club_code"] = df["club_code"].replace(CODIGOS_EQUIPO_ACTUALES)
    df["week"] = df["week"].astype(int)
    df["depth_team"] = df["depth_team"].astype(int)
    df = df.sort_values("depth_team").drop_duplicates(
        subset=["season", "week", "club_code", "gsis_id"], keep="first"
    )
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


def cargar_depth_charts_unificado(seasons, calendario=None, posicion="WR"):
    """Une cargar_depth_charts_historico() (<=2024) y el esquema nuevo de
    nflreadpy (2025 en adelante, incluida la temporada en curso) en un solo
    DataFrame (season, week, team, player_id, depth_team) -- resuelve la
    limitacion documentada desde Fase 2/4 (depth_team 100% nulo en 2025+,
    ver HALLAZGOS.md).

    El esquema nuevo no trae season/week, solo `dt` (fecha/hora del scrape).
    Se infiere la semana con la misma logica real que usa un depth chart:
    un scrape describe la alineacion que el equipo esta preparando para su
    PROXIMO partido, no el que ya jugo -- se asigna cada `dt` al primer
    partido de ese equipo en o despues de esa fecha (pd.merge_asof,
    direction="forward", sobre el calendario real, que ya incluye partidos
    programados aun no jugados -- por eso esto tambien sirve para la
    alineacion "de hoy" al predecir la proxima semana no jugada, ver
    notebooks/6.1_modelo_final_y_temporada_actual.ipynb). De todos los `dt` que
    caen en la misma ventana (equipo-semana) se usa solo el mas reciente --
    el mas cercano al partido es el mas confiable/final. `pos_rank` (2025+,
    ranking limpio sin empates dentro del equipo, a diferencia de
    depth_team en el esquema viejo) hace el mismo papel que `depth_team`.

    Verificado con casos reales (notebooks/1.3_unificacion_alineaciones.ipynb):
    Marvin Harrison Jr. (ARI) y Ja'Marr Chase (CIN) salen como WR1 la
    inmensa mayoria de semanas de 2025, igual que se sabe que jugaron en la
    realidad; cobertura sobre filas WR reales de 2025 sube de 0% a ~97%; sin
    filas duplicadas jugador-equipo-semana.

    `calendario`: se puede pasar ya cargado (cargar_calendario) para no
    repetir la consulta si quien llama ya lo tiene -- se carga aqui solo si
    hace falta."""
    seasons = list(seasons)
    viejas = [s for s in seasons if s <= 2024]
    nuevas = [s for s in seasons if s >= 2025]
    partes = []

    if viejas:
        hist = cargar_depth_charts_historico(viejas, posicion=posicion)
        hist = hist.rename(columns={"club_code": "team", "gsis_id": "player_id"})
        partes.append(hist[["season", "week", "team", "player_id", "depth_team"]])

    if nuevas:
        if calendario is None:
            calendario = cargar_calendario(nuevas)
        else:
            calendario = calendario[calendario["season"].isin(nuevas)]
        d = nfl.load_depth_charts(nuevas)
        d = d.filter(d["pos_abb"] == posicion)
        df = d.to_pandas()
        df["dt"] = pd.to_datetime(df["dt"]).dt.tz_localize(None)

        cal_home = calendario[["season", "week", "gameday", "home_team"]].rename(
            columns={"home_team": "team"}
        )
        cal_away = calendario[["season", "week", "gameday", "away_team"]].rename(
            columns={"away_team": "team"}
        )
        partidos = pd.concat([cal_home, cal_away], ignore_index=True)

        asignado = []
        for team, grupo in df.groupby("team"):
            partidos_team = partidos[partidos["team"] == team].sort_values("gameday")
            if partidos_team.empty:
                continue
            asignado.append(
                pd.merge_asof(
                    grupo.sort_values("dt"),
                    partidos_team[["gameday", "season", "week"]],
                    left_on="dt",
                    right_on="gameday",
                    direction="forward",
                )
            )
        df = pd.concat(asignado, ignore_index=True)
        # dt posteriores al ultimo partido de la temporada (post-temporada,
        # offseason) no tienen un "proximo partido" -- se descartan, no
        # corresponden a ninguna semana real.
        df = df.dropna(subset=["season", "week"])

        ultimo_dt = df.groupby(["season", "week", "team"])["dt"].transform("max")
        df = df[df["dt"] == ultimo_dt]
        df = df.rename(columns={"gsis_id": "player_id", "pos_rank": "depth_team"})
        partes.append(df[["season", "week", "team", "player_id", "depth_team"]])

    return pd.concat(partes, ignore_index=True)


def identificar_qb_titular(stats, id_cols=("season", "week", "team")):
    """QB titular de un equipo en una semana: el pasador con mas intentos de pase
    esa semana, sobre cargar_stats_semanales() -- no hace falta cargar_jugadas(),
    attempts/passing_epa ya vienen en el resumen semanal.

    Empate de attempts: verificado sobre 2016-2025, ocurre en 7 de 5269
    equipo-semana (0.13%) -- raro pero real, se desempata por passing_epa (mejor
    desempeño esa semana), no por orden arbitrario de fila.

    Limite conocido: funciona sobre partidos ya jugados (attempts real). Para la
    semana en curso, sin jugarse, no hay attempts todavia -- se necesitaria el
    titular esperado (depth chart/reporte de lesiones), mismo caso que ya separa
    cargar_depth_charts_historico de cargar_depth_charts_actual."""
    cols = list(id_cols)
    qb = stats[(stats["position"] == "QB") & (stats["attempts"] > 0)][
        cols + ["player_id", "player_display_name", "attempts", "passing_epa"]
    ].copy()
    qb = qb.sort_values(["attempts", "passing_epa"], ascending=False)
    titular = qb.drop_duplicates(subset=cols)
    titular = titular.rename(
        columns={
            "player_id": "qb_id",
            "player_display_name": "qb_nombre",
            "passing_epa": "qb_passing_epa",
        }
    )
    return titular[cols + ["qb_id", "qb_nombre", "qb_passing_epa"]]
