"""
Feature engineering para el pipeline de WR semanal.

Reemplaza la logica que hoy vive triplicada (una copia casi identica en cada
uno de wrs_rec_tds.ipynb, wrs_rec_yds.ipynb, wrs_receptions.ipynb, ver
weekly/wr/notebooks/_reference/) por un solo lugar.

target_share, air_yards_share y wopr NO se calculan aqui -- ya vienen nativos
y limpios en cargar_stats_semanales() (0% nulo para WR, ver DATASHEET.md). El
pipeline heredado los recalculaba a mano desde play-by-play dos veces por
notebook; con nflreadpy eso ya no hace falta.
"""
import pandas as pd


def _promedio_expandido_desfasado(serie):
    """Promedio de todo lo anterior a la fila actual -- NUNCA incluye el
    valor de la fila misma. shift(1) primero, expanding() despues."""
    return serie.shift(1).expanding().mean()


def _promedio_movil_desfasado(serie, ventana):
    """Promedio de las `ventana` filas anteriores -- tampoco incluye la fila
    actual. min_periods=1 para que las primeras filas del jugador no queden
    todas en NaN por falta de historia completa."""
    return serie.shift(1).rolling(ventana, min_periods=1).mean()


def agregar_promedios_jugador(stats, columnas, ventanas=(3, 5), id_col="player_id"):
    """Agrega, para cada columna en `columnas`, los promedios del propio
    jugador calculados SOLO con semanas anteriores a la fila actual:

    - {columna}_season_avg : promedio de la temporada en curso, hasta antes
      de esta semana (se reinicia cada temporada).
    - {columna}_last{N}_avg : un promedio movil por cada N en `ventanas` (por
      default 3 y 5), de las ultimas N apariciones del jugador, sin reiniciar
      por temporada (refleja forma reciente real, no un corte artificial en
      el calendario).
    - {columna}_career_avg : promedio de toda la carrera del jugador hasta
      antes de esta semana.

    No se decide aqui cual ventana (3, 5, u otra) es "la correcta" -- se
    calculan todas las que se pidan y se deja que el analisis/modelo de las
    fases siguientes muestre con evidencia cual aporta, igual que ya se hizo
    con Vegas/clima (se probaron, no se asumieron).

    Se usa groupby(...).transform() -- no groupby().apply() con
    reset_index manual, que es exactamente el patron que causo el bug de
    fuga de datos ya documentado en docs/HALLAZGOS.md (career_avg.ipynb y
    los 3 wrs_rec_*.ipynb heredados). transform() conserva el indice
    original automaticamente, sin ese riesgo.

    Requiere que `stats` este ordenado o se ordena aqui por
    (id_col, season, week) antes de calcular -- el orden es lo que hace que
    "shift(1)" signifique "la aparicion anterior de este jugador", no una
    fila cualquiera."""
    df = stats.sort_values([id_col, "season", "week"]).copy()

    for col in columnas:
        df[f"{col}_season_avg"] = df.groupby([id_col, "season"])[col].transform(
            _promedio_expandido_desfasado
        )
        for ventana in ventanas:
            df[f"{col}_last{ventana}_avg"] = df.groupby(id_col)[col].transform(
                lambda s, v=ventana: _promedio_movil_desfasado(s, v)
            )
        df[f"{col}_career_avg"] = df.groupby(id_col)[col].transform(
            _promedio_expandido_desfasado
        )

    return df
