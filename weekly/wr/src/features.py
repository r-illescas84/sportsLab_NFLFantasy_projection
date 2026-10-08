"""
Feature engineering para el pipeline de WR semanal.

Reemplaza la logica que hoy vive triplicada (una copia casi identica en cada
uno de wrs_rec_tds.ipynb, wrs_rec_yds.ipynb, wrs_receptions.ipynb, ver
weekly/wr/notebooks/_reference/) por un solo lugar.

target_share, air_yards_share y wopr NO se calculan aqui -- ya vienen nativos
y limpios en cargar_stats_semanales() (0% nulo para WR, ver DATASHEET.md). El
pipeline heredado los recalculaba a mano desde play-by-play dos veces por
notebook; con nflreadpy eso ya no hace falta.

Este modulo corre cada semana (pipeline.py) y solo contiene lo que alimenta a los modelos
vigentes. Las candidatas que se probaron y no entraron (volatilidad, cambio de QB o de equipo,
otras interacciones, topes) viven en features_exploratorio.py y solo las usan los notebooks.

Seleccion de variables -- que variable, que decision, con que evidencia. Construccion y
verificacion de no-fuga en notebooks/3.1_ingenieria_features.ipynb; seleccion en
notebooks/3.3_seleccion_estable.ipynb (docs/decisions/0005-seleccion-de-variables.md): las 110
candidatas se ordenan por eliminacion recursiva con importancia por permutacion (metrica principal
de cada resultado, ajuste 2016-2019, medicion 2020-2021, 10 submuestras) y se elige el conjunto
mas chico que, con 95% de confianza, no es mas de 1% peor que el mejor en validacion 2022-2023 en
ninguno de los tres resultados: las 25 primeras. La seleccion anterior (34 variables,
notebooks/3.2_seleccion_features.ipynb) queda como registro en
features_exploratorio.VARIABLES_SELECCION_3_2.

| Variable | Que es | Decision | Evidencia |
|----------|--------|----------|-----------|
| Volumen y participacion del receptor (recepciones, objetivos, yardas, yardas tras la recepcion, primeros downs, target_share, air_yards_share, wopr, puntos de fantasy) | Lo que produce y cuanto lo usa su equipo, en distintas ventanas | Incluir las que quedan entre las 25 primeras del orden estable | Son el grueso del conjunto; estan muy correlacionadas entre si (un grupo de 53 candidatas con correlacion media de 0.7 o mas, nb 3.3), por eso se ordenaron con eliminacion recursiva |
| depth_team | Su lugar en la alineacion del equipo (1 = titular) | Incluir | Segunda del orden estable (nb 3.3); cobertura en 2025+ via data.cargar_depth_charts_unificado (nb 1.3) |
| total_implicito_equipo | Puntos que las lineas de apuestas esperan que anote su equipo | Incluir | Octava del orden estable, estable entre submuestras (nb 3.3); se conoce antes del partido (formula en nb 2.3) |
| target_share_x_ofensiva_equipo | Participacion reciente por yardas de recepcion de los WR de su equipo en la temporada | Incluir | Entre las 25 primeras (nb 3.3) |
| draft_pick, edad | Numero de seleccion en el draft (imputado: peor pick real + 1) y edad | Incluir | Entre las 25 primeras (nb 3.3) |
| anios_experiencia (+ _sq/_bucket) | Temporadas en la NFL | No en arboles; si en lineales | Lugar 78 del orden estable (nb 3.3); los modelos lineales la usan en version no lineal por la curva de nb 2.3 |
| target_share/air_yards_share/wopr juntas en Ridge | Las tres miden participacion, muy correlacionadas | Usar columnas_por_modelo("ridge") | Colinealidad 0.82-0.97 (nb 2.5) |
| Volatilidad, cambio de QB o de equipo, rest, div_game, roof, surface, otras interacciones | Contexto y consistencia | No incluir | Fuera de las 25 primeras (nb 3.3); el cambio de QB ademas no se conoce antes del partido |
| Dureza defensiva del rival, clima, acarreos de WR | Que tan dificil es el rival contra WR; temperatura y viento; jet sweeps | No incluir | Sin efecto en el analisis exploratorio (nb 2.3 a 2.5); no se pasaron a la seleccion |
"""
import numpy as np
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
    fases siguientes muestre con evidencia cual aporta.

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


def calcular_ratios_eficiencia(stats):
    """Ratios de eficiencia por semana (no de volumen): yards_per_target,
    catch_rate, air_yards_per_target. Se calculan aqui crudos -- despues se
    rezagan con agregar_promedios_jugador() como cualquier otra columna de
    rendimiento, sin duplicar logica de shift/rolling.

    0 en el denominador (semana sin targets) da NaN, no error ni infinito -- un
    jugador sin targets esa semana no tiene una tasa de acierto que calcular."""
    df = stats.copy()
    con_targets = df["targets"] > 0
    df["yards_per_target"] = (df["receiving_yards"] / df["targets"]).where(con_targets)
    df["catch_rate"] = (df["receptions"] / df["targets"]).where(con_targets)
    df["air_yards_per_target"] = (df["receiving_air_yards"] / df["targets"]).where(con_targets)
    return df


def agregar_edad_experiencia(stats, jugadores, id_col="player_id"):
    """edad al 1 de septiembre de la temporada correspondiente, anios_experiencia
    = temporada - temporada de novato (misma convencion que 2.3_eda_general.ipynb)."""
    df = stats.merge(
        jugadores[["gsis_id", "birth_date", "rookie_season"]],
        left_on=id_col,
        right_on="gsis_id",
        how="left",
    )
    referencia = pd.to_datetime(df["season"].astype(str) + "-09-01")
    df["edad"] = (referencia - df["birth_date"]).dt.days / 365.25
    df["anios_experiencia"] = df["season"] - df["rookie_season"]
    return df.drop(columns=["gsis_id", "birth_date", "rookie_season"])


def agregar_experiencia_no_lineal(stats, col="anios_experiencia"):
    """Termino cuadratico + bucket de anios_experiencia -- la relacion real tiene
    pico en 3-5 anios y baja despues, confirmado en los 3 targets
    (2.3_eda_general.ipynb); una correlacion lineal no lo detecta, un termino
    cuadratico o un bucket si pueden. Cortes identicos a los ya validados con
    datos reales en ese notebook -- no se inventan de nuevo."""
    df = stats.copy()
    df[f"{col}_sq"] = df[col] ** 2
    df[f"{col}_bucket"] = pd.cut(
        df[col],
        bins=[-1, 0, 2, 5, 100],
        labels=["Rookie", "1-2 años", "3-5 años", "6+ años"],
    )
    return df


def agregar_draft_pick(stats, jugadores, id_col="player_id"):
    """draft_pick es NaN estructural para jugadores no drafteados (40.3% de los
    receptores de la tabla de modelado, glosario seccion 7.3) -- no es un dato faltante por error, es la
    ausencia real de un pick. Se imputa como peor que el ultimo pick real
    observado (max + 1, calculado sobre el dato, no un numero fijo): un no
    drafteado es, por definicion, peor que el ultimo jugador drafteado, no un
    caso "promedio" que ameritara imputar con la media o la mediana."""
    df = stats.merge(
        jugadores[["gsis_id", "draft_pick"]],
        left_on=id_col,
        right_on="gsis_id",
        how="left",
    )
    peor_que_ultimo = jugadores["draft_pick"].max() + 1
    df["draft_pick"] = df["draft_pick"].fillna(peor_que_ultimo)
    return df.drop(columns=["gsis_id"])


def agregar_ofensiva_equipo(stats, columnas=("receiving_yards",), ventanas=(3, 5), team_col="team"):
    """Agregado de equipo-semana (suma de todos los WR de ese equipo esa semana), rezagado con
    agregar_promedios_jugador(id_col=team_col) -- misma logica sin fuga. Columnas
    equipo_{columna}_{ventana}_avg. La estabilidad de 0.40 de 2.4_eda_equipos.ipynb es anual
    (equipo-temporada); dentro de la temporada se relaciona poco con la produccion individual
    (2.5_eda_multivariable.ipynb), pero combinada con la participacion del receptor
    (target_share_x_ofensiva_equipo) entra al modelo (3.3_seleccion_estable.ipynb). En una semana
    pendiente, las filas sin estadisticas suman 0 para esa semana, pero sus variables usan solo las
    semanas anteriores."""
    equipo_semana = stats.groupby(["season", team_col, "week"], as_index=False)[list(columnas)].sum()
    equipo_semana = agregar_promedios_jugador(equipo_semana, columnas, ventanas=ventanas, id_col=team_col)
    nuevas = [c for c in equipo_semana.columns if c not in ["season", team_col, "week"] + list(columnas)]
    equipo_semana = equipo_semana.rename(columns={c: f"equipo_{c}" for c in nuevas})
    return stats.merge(
        equipo_semana[["season", team_col, "week"] + [f"equipo_{c}" for c in nuevas]],
        on=["season", team_col, "week"],
        how="left",
    )


def agregar_interaccion(stats, col_a, col_b, nombre=None):
    """Producto de dos columnas ya rezagadas, con nombre `nombre` (por defecto col_a_x_col_b)."""
    df = stats.copy()
    df[nombre or f"{col_a}_x_{col_b}"] = df[col_a] * df[col_b]
    return df


def agregar_total_implicito(stats, calendario, team_col="team"):
    """Total implicito de puntos del equipo segun las lineas de apuestas del calendario:
    (total + linea) / 2 para el local y (total - linea) / 2 para el visitante (spread_line
    positivo = el local es favorito; 2.3_eda_general.ipynb). Se conoce antes del partido. Las
    lineas historicas son las de cierre; para una semana pendiente, las vigentes al consultar el
    calendario."""
    columnas = ["season", "week", "spread_line", "total_line"]
    local = calendario[columnas + ["home_team"]].rename(columns={"home_team": team_col})
    visita = calendario[columnas + ["away_team"]].rename(columns={"away_team": team_col})
    local["total_implicito_equipo"] = (local["total_line"] + local["spread_line"]) / 2
    visita["total_implicito_equipo"] = (visita["total_line"] - visita["spread_line"]) / 2
    lineas = pd.concat([local, visita], ignore_index=True)[["season", "week", team_col, "total_implicito_equipo"]]
    filas = len(stats)
    df = stats.merge(lineas, on=["season", "week", team_col], how="left")
    assert len(df) == filas, "el cruce con el calendario no debe duplicar filas"
    return df


COLUMNAS_BASE_MODELADO = [
    "receptions", "targets", "receiving_yards", "receiving_yards_after_catch", "receiving_first_downs",
    "target_share", "air_yards_share", "wopr", "fantasy_points_ppr", "receiving_tds",
]
"""Columnas a las que se les calculan los promedios rezagados (agregar_promedios_jugador): las
estadisticas de las que salen las variables de FEATURES_SELECCIONADAS_ARBOL, mas receiving_tds,
cuyos promedios usa la referencia de los notebooks (experimentos.BaselinePromedioJugador)."""

FEATURES_SELECCIONADAS_ARBOL = [
    "receptions_career_avg", "depth_team", "target_share_last5_avg", "receptions_season_avg",
    "target_share_last3_avg", "receptions_last3_avg", "fantasy_points_ppr_last5_avg", "total_implicito_equipo",
    "receiving_yards_after_catch_career_avg", "receiving_yards_career_avg", "wopr_last3_avg",
    "target_share_season_avg", "receiving_yards_last5_avg", "fantasy_points_ppr_career_avg",
    "receptions_last5_avg", "fantasy_points_ppr_last3_avg", "wopr_season_avg", "target_share_x_ofensiva_equipo",
    "targets_season_avg", "draft_pick", "receiving_first_downs_career_avg", "air_yards_share_last3_avg",
    "receiving_yards_after_catch_season_avg", "targets_last5_avg", "edad",
]
"""Las 25 variables de los modelos de arbol, en el orden estable de 3.3_seleccion_estable.ipynb
(la primera es la que mas dura en la eliminacion recursiva)."""

_COLINEALES_PARTICIPACION = {
    "target_share_last5_avg", "target_share_last3_avg", "target_share_season_avg", "air_yards_share_last3_avg",
}

_EXPERIENCIA_NO_LINEAL = ["anios_experiencia", "anios_experiencia_sq", "anios_experiencia_bucket"]


def columnas_por_modelo(tipo="ridge"):
    """Variables por familia de modelo. 'arbol': las 25 de FEATURES_SELECCIONADAS_ARBOL -- la
    colinealidad de target_share/air_yards_share/wopr (0.82-0.97, 2.5_eda_multivariable.ipynb) no
    perjudica a RandomForest/XGBoost/HistGradientBoosting. 'ridge' (solo notebooks): se quitan las
    versiones de target_share/air_yards_share (se conserva wopr, que ya las combina) y se agrega la
    experiencia en version no lineal (lineal, cuadratica y por tramos), que un modelo lineal
    necesita para captar la curva de 2.3_eda_general.ipynb y un arbol no."""
    if tipo == "arbol":
        return list(FEATURES_SELECCIONADAS_ARBOL)
    if tipo == "ridge":
        base = [c for c in FEATURES_SELECCIONADAS_ARBOL if c not in _COLINEALES_PARTICIPACION]
        return base + _EXPERIENCIA_NO_LINEAL
    raise ValueError(f"tipo desconocido: {tipo!r}")


def construir_tabla_modelado(seasons=range(2016, 2026), filas_extra=None):
    """Arma la tabla WR por jugador-semana con las variables que usan los
    modelos vigentes (columnas_por_modelo()). Es el unico punto de entrada de
    features para pipeline.py y para los notebooks de modelado.

    `filas_extra`: DataFrame opcional con filas "placeholder" (estadisticas en
    NaN, ver filas_de_la_semana()) para una semana que todavia no se juega --
    se agregan a `stats` ANTES de calcular cualquier promedio, para que
    agregar_promedios_jugador() (shift(1) antes de promediar) les calcule sus
    variables rezagadas usando solo la historia real ya jugada, sin duplicar la
    logica en otro lugar. Se marcan con `es_prediccion=True` para separarlas del
    resto de filas reales (`es_prediccion=False`).

    Requiere `data.py` en el mismo path (sys.path.insert(0, "../src"); import
    data, features)."""
    import data

    seasons = list(seasons)
    stats = data.cargar_stats_semanales(seasons)
    jugadores = data.cargar_jugadores()
    calendario = data.cargar_calendario(seasons)
    depth = data.cargar_depth_charts_unificado(seasons, calendario=calendario)
    wr = stats[stats["position"] == "WR"].copy()
    wr["es_prediccion"] = False
    if filas_extra is not None:
        filas_extra = filas_extra.copy()
        filas_extra["es_prediccion"] = True
        wr = pd.concat([wr, filas_extra], ignore_index=True)

    wr = agregar_promedios_jugador(wr, COLUMNAS_BASE_MODELADO, ventanas=(3, 5))
    wr = agregar_ofensiva_equipo(wr, columnas=("receiving_yards",), ventanas=(3, 5))
    wr = agregar_interaccion(wr, "target_share_last5_avg", "equipo_receiving_yards_season_avg",
                             "target_share_x_ofensiva_equipo")
    wr = agregar_edad_experiencia(wr, jugadores)
    wr = agregar_experiencia_no_lineal(wr)
    wr = agregar_draft_pick(wr, jugadores)
    wr = agregar_total_implicito(wr, calendario)
    wr = wr.merge(
        depth[["season", "week", "team", "player_id", "depth_team"]],
        on=["season", "week", "team", "player_id"],
        how="left",
    )
    return wr


def filas_de_la_semana(season, week):
    """Filas "placeholder" de los WR a predecir en una semana que todavia no se
    juega: los que estan en la alineacion de esa semana
    (data.cargar_depth_charts_unificado) y ya jugaron al menos un partido en la
    temporada actual o la anterior. Sin historia no hay variables rezagadas que
    calcular, asi que un WR sin ningun partido previo (ej. un novato en su
    primera semana) no se predice. Estadisticas en NaN, listas para pasar a
    construir_tabla_modelado(filas_extra=...)."""
    import data

    stats = data.cargar_stats_semanales([season - 1, season])
    con_historia = set(stats.loc[stats["position"] == "WR", "player_id"])

    depth = data.cargar_depth_charts_unificado([season])
    depth = depth[(depth["season"] == season) & (depth["week"] == week)]
    depth = depth[depth["player_id"].isin(con_historia)]
    depth = depth.sort_values("depth_team").drop_duplicates("player_id")

    nombres = data.cargar_jugadores()[["gsis_id", "display_name"]].rename(
        columns={"gsis_id": "player_id", "display_name": "player_display_name"}
    )
    filas = depth[["player_id", "team"]].merge(nombres, on="player_id", how="left")
    filas["season"] = season
    filas["week"] = week
    filas["position"] = "WR"
    for columna in COLUMNAS_BASE_MODELADO:
        filas[columna] = np.nan
    return filas.reset_index(drop=True)
