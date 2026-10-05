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
vigentes. Las variables de la Fase 4 que se probaron y no entraron (volatilidad, interacciones,
cambio de QB, ofensiva de equipo, topes) viven en features_exploratorio.py y solo las usan
los notebooks.

Seleccion de features (Fase 4) -- que variable, que decision, con que evidencia.
Construccion y verificacion de no-fuga en notebooks/10_ingenieria_features.ipynb;
evaluacion conjunta contra un modelo (no solo correlacion aislada) en
notebooks/11_seleccion_features.ipynb:

| Variable | Que es | Decision | Evidencia |
|----------|--------|----------|-----------|
| target_share/wopr/air_yards_share/receiving_epa/fantasy_points_ppr/receiving_first_downs (last5/season) | Que tanto del trabajo de pase del equipo recae en el jugador (participacion + eficiencia), resumido en sus ultimos 5 partidos o en lo que va de temporada | Incluir, prioritarias | Maxima importancia conjunta en los 3 targets (nb 11, secciones 4.2 y 5) |
| Resto de columnas de uso/rendimiento (last5/season), incl. receiving_air_yards, receiving_10/16/20 | Volumen/produccion propia del jugador (recepciones, yardas, targets, etc.), mismo resumen por ventana | Incluir | Mismo patron, importancia real -- nombradas individualmente en nb 11 seccion 4.2 tras aparecer en el top 15 de al menos un target |
| Ventana last3 | Promedio de sus ultimos 3 partidos | Incluir, secundaria | Aporta menos que last5/season en los 3 targets (nb 11) |
| Ventana career | Promedio de toda su carrera hasta antes de esa semana | Conservar, no priorizar en modelado | Redundante frente a last5+season en conjunto, pese a buena correlacion aislada en Fase 3 (nb 11) |
| target_share/air_yards_share/wopr juntas en Ridge | Las 3 miden participacion del jugador de formas distintas, muy correlacionadas entre si | Usar columnas_por_modelo("ridge") | Colinealidad 0.82-0.97 (nb 08, Fase 3) |
| depth_team | Su lugar en la alineacion del equipo (1=titular/WR1, 2=WR2...) -- mas bajo es mejor | Incluir | Señal fuerte y nueva en recepciones/yardas (nb 11); cobertura en 2025+ resuelta via data.cargar_depth_charts_unificado (nb 21, ~97% de cobertura real, antes 0%) |
| draft_pick | En que numero fue seleccionado en el draft (mas bajo = elegido antes); al no drafteado se le asigna peor que el ultimo pick real | Incluir (imputado: peor pick real + 1) | Confirmado con evidencia de modelo, no solo correlacion (nb 11) |
| edad / anios_experiencia (+ _sq/_bucket para lineales) | Edad del jugador esa temporada / cuantas temporadas lleva en la NFL | Incluir | Confirmado con evidencia de modelo (nb 11); transformacion no lineal es especifica para Ridge |
| racr_acotado | Eficiencia de conversion de yardas aereas (yardas recibidas / yardas de aire), con tope para evitar valores extremos | Se mantiene calculado, sin evidencia fuerte | No destaco en la evaluacion conjunta (nb 11) |
| Volatilidad reciente (_volatilidad5) | Que tan parejo o irregular fue el jugador en sus ultimas 5 apariciones (alta = boom-bust, baja = consistente) | No incluir por ahora | Importancia nula/negativa en los 3 targets (nb 11) |
| cambio_qb_titular / cambio_equipo | Si cambio el QB titular de su equipo, o si el jugador cambio de equipo, respecto a su aparicion anterior | No incluir en el feature set principal | Aporte marginal casi nulo una vez presentes las features de uso reciente (nb 11) -- matiza, no invalida, el hallazgo bivariado de Fase 3 (nb 08) |
| Interacciones (target_share_x_ofensiva_equipo, draft_pick_x_experiencia, cambio_qb_x_target_share) | Combinan 2 variables ya existentes cada una (uso x ofensiva de equipo, draft x experiencia, cambio de QB x uso) | No incluir | Sin aporte claro, una incluso negativa (nb 11) |
| Dureza defensiva del rival, Vegas/clima, acarreos de WR | Que tan dificil es el rival contra WR; puntos esperados por las casas de apuestas y clima; jugadas de carrera de un WR (jet sweeps) | No incluir | Confirmado sin efecto en Fase 3, no reevaluado sin razon nueva |
| rest / div_game                                         | No incluir                             | Sin evidencia de aportar (nb 11) |
| Filas sin ningun target esa semana (targets == 0)       | Decision de modelado, no de features -- se deja para Fase 5 | Afecta la funcion de perdida (Poisson/Tweedie ya sugerido en nb 05), no que columnas usar |
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
    = temporada - temporada de novato -- misma convencion ya usada en
    preeliminar/03_modelo_predictivo y 06_eda_general.ipynb, para poder comparar
    criterios entre ambos proyectos."""
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
    (06_eda_general.ipynb); una correlacion lineal no lo detecta, un termino
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
    """draft_pick es NaN estructural para jugadores no drafteados (54% de los WR
    historicos, ver DATASHEET.md) -- no es un dato faltante por error, es la
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


COLUMNAS_BASE_MODELADO = [
    "receptions", "targets", "receiving_yards", "receiving_air_yards",
    "receiving_first_downs", "receiving_tds", "receiving_10", "receiving_16", "receiving_20",
    "target_share", "air_yards_share", "wopr", "receiving_epa", "fantasy_points_ppr",
    "catch_rate",
]
"""Columnas a las que se les calculan los promedios rezagados
(agregar_promedios_jugador): solo las 15 de las que salen las variables de
FEATURES_SELECCIONADAS_ARBOL. catch_rate no viene de nflreadpy, la deriva
calcular_ratios_eficiencia()."""

FEATURES_SELECCIONADAS_ARBOL = sorted({
    # Union de las 15 variables mas importantes de cada uno de los 3 targets
    # (11_seleccion_features.ipynb, seccion 4.2 -- 32 variables, evidencia real).
    "depth_team", "draft_pick", "target_share_last5_avg", "wopr_last5_avg",
    "fantasy_points_ppr_last5_avg", "targets_season_avg", "receptions_season_avg",
    "receiving_yards_season_avg", "receiving_first_downs_last5_avg", "wopr_last3_avg",
    "targets_last5_avg", "air_yards_share_season_avg", "receiving_yards_career_avg",
    "receiving_yards_last3_avg", "receiving_air_yards_season_avg", "targets_last3_avg",
    "receptions_last3_avg", "receiving_10_last3_avg", "fantasy_points_ppr_season_avg",
    "receiving_tds_season_avg", "receiving_16_last5_avg", "receiving_epa_last3_avg",
    "receptions_career_avg", "fantasy_points_ppr_last3_avg", "catch_rate_career_avg",
    "receiving_20_last3_avg", "receiving_first_downs_career_avg", "target_share_last3_avg",
    "receptions_last5_avg", "target_share_career_avg", "air_yards_share_career_avg",
    "air_yards_share_last3_avg",
    # Confirmadas con evidencia de modelo individual aunque quedaron justo fuera
    # del top 15 (11_seleccion_features.ipynb, seccion 6): edad 0.067/0.003/0.001,
    # anios_experiencia 0.017/0.000/0.000 en yardas/recepciones/TDs.
    "edad", "anios_experiencia",
})
"""Feature set de arbol (RF/XGBoost/HistGradientBoosting) para Fase 5 -- 34
columnas. No incluye `career_avg` (redundante frente a last5+season en
conjunto, ver seccion 5 de 11_seleccion_features.ipynb) ni las variables
descartadas (volatilidad, cambio de QB/equipo, interacciones, rest/div_game,
dureza defensiva, Vegas/clima)."""

_COLINEALES_PARTICIPACION = {
    "target_share_last5_avg", "target_share_last3_avg", "target_share_career_avg",
    "air_yards_share_season_avg", "air_yards_share_last3_avg", "air_yards_share_career_avg",
}


def columnas_por_modelo(tipo="ridge"):
    """Feature set completo de Fase 5 por familia de modelo. 'arbol': las 34
    de FEATURES_SELECCIONADAS_ARBOL -- la colinealidad de target_share/
    air_yards_share/wopr (0.82-0.97, 08_eda_multivariable.ipynb) no perjudica
    a RandomForest/XGBoost/HistGradientBoosting. 'ridge': se quitan las
    versiones de target_share/air_yards_share (se conserva wopr, que ya las
    combina, mas receiving_epa -- la mas independiente del grupo) y se agrega
    la version no lineal de experiencia (anios_experiencia_sq/_bucket), que
    solo un modelo lineal necesita -- un arbol ya captura la curva del dato
    crudo."""
    if tipo == "arbol":
        return list(FEATURES_SELECCIONADAS_ARBOL)
    if tipo == "ridge":
        base = [c for c in FEATURES_SELECCIONADAS_ARBOL if c not in _COLINEALES_PARTICIPACION]
        return base + ["anios_experiencia_sq", "anios_experiencia_bucket"]
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

    wr = calcular_ratios_eficiencia(wr)
    wr = agregar_promedios_jugador(wr, COLUMNAS_BASE_MODELADO, ventanas=(3, 5))
    wr = agregar_edad_experiencia(wr, jugadores)
    wr = agregar_experiencia_no_lineal(wr)
    wr = agregar_draft_pick(wr, jugadores)
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
        if columna != "catch_rate":
            filas[columna] = np.nan
    return filas.reset_index(drop=True)
