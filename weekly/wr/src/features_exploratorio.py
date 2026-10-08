"""
Variables candidatas que se construyeron y evaluaron pero NO entraron al modelo
(tabla de decisiones en features.py; evidencia en 3.1_ingenieria_features.ipynb,
3.2_seleccion_features.ipynb y 3.3_seleccion_estable.ipynb), y la tabla completa de
candidatas de la seleccion de variables.

Solo se usan desde los notebooks. El flujo semanal (pipeline.py) no las calcula: el
modelo vigente solo usa las variables de features.FEATURES_SELECCIONADAS_ARBOL.
"""
import pandas as pd

import features
from features import agregar_interaccion, agregar_ofensiva_equipo, agregar_total_implicito  # noqa: F401


def acotar_variable(stats, columna, limite=None, percentil=0.99):
    """Topa una columna inestable sobre su valor semanal CRUDO, antes de
    promediar -- evidencia: racr promediado empeora su curtosis (241 crudo -> 605
    promediado, ver 2.2_eda_perfil_variables.ipynb), un solo valor disparado
    arrastra el promedio de todo un jugador. limite=None calcula el percentil
    real de la columna en vez de un numero inventado."""
    df = stats.copy()
    tope = df[columna].quantile(percentil) if limite is None else limite
    df[f"{columna}_acotado"] = df[columna].clip(upper=tope)
    return df


def agregar_volatilidad_jugador(stats, columnas, ventana=5, id_col="player_id"):
    """Desviacion estandar de las ultimas `ventana` apariciones del jugador --
    mide consistencia/boom-bust, no solo nivel promedio (conectado con el
    hallazgo de 2.6_eda_jugador_destacado.ipynb sobre semanas boom). shift(1)
    antes de rolling().std(), mismo patron leak-safe que
    agregar_promedios_jugador(). min_periods=2: un desvio con una sola
    observacion no esta definido."""
    df = stats.sort_values([id_col, "season", "week"]).copy()
    for col in columnas:
        df[f"{col}_volatilidad{ventana}"] = df.groupby(id_col)[col].transform(
            lambda s: s.shift(1).rolling(ventana, min_periods=2).std()
        )
    return df


def agregar_cambio_qb_titular(stats, qb_titular, id_col="player_id", team_col="team"):
    """Cruza data.identificar_qb_titular() y marca si el QB titular del equipo
    del jugador cambio respecto a SU aparicion anterior (shift(1) agrupado por
    id_col, leak-safe). Primera semana de carrera queda NaN, no False -- no hay
    'anterior' con quien comparar; lo mismo si no se identifico titular en
    alguna de las 2 semanas.

    Un traspaso de equipo a media temporada (confirmado real: 105 filas de WR en
    2016-2025) tambien queda marcado como cambio de QB titular -- es literalmente
    cierto, un jugador que cambia de equipo casi siempre cambia de QB. Se agrega
    ademas `cambio_equipo` por separado para que la fase de seleccion pueda
    distinguir ambas senales en vez de mezclarlas sin querer."""
    df = stats.merge(qb_titular, on=["season", "week", team_col], how="left")
    df = df.sort_values([id_col, "season", "week"])
    df["qb_anterior"] = df.groupby(id_col)["qb_id"].shift(1)
    df["team_anterior"] = df.groupby(id_col)[team_col].shift(1)
    hay_historia = df["qb_anterior"].notna() & df["qb_id"].notna()
    df["cambio_qb_titular"] = ((df["qb_id"] != df["qb_anterior"]) & hay_historia).where(
        hay_historia
    )
    df["cambio_equipo"] = (
        (df[team_col] != df["team_anterior"]) & df["team_anterior"].notna()
    ).where(df["team_anterior"].notna())
    return df.drop(columns=["qb_anterior", "team_anterior"])


COLUMNAS_BASE_CANDIDATAS = [
    "receptions", "targets", "receiving_yards", "receiving_air_yards", "receiving_yards_after_catch",
    "receiving_first_downs", "receiving_tds", "receiving_2pt_conversions", "receiving_10", "receiving_16",
    "receiving_20", "receiving_40", "target_share", "air_yards_share", "wopr", "receiving_epa",
    "fantasy_points_ppr", "yards_per_target", "catch_rate", "air_yards_per_target", "racr_acotado",
]
"""Las 21 estadisticas semanales que entran como candidatas en sus cuatro ventanas
(VENTANAS_CANDIDATAS): las mismas de 3.2_seleccion_features.ipynb."""

VENTANAS_CANDIDATAS = ("_season_avg", "_last3_avg", "_last5_avg", "_career_avg")

CANDIDATAS_CONTEXTO = [
    "receiving_yards_volatilidad5", "targets_volatilidad5",
    "edad", "anios_experiencia", "anios_experiencia_sq", "draft_pick",
    "rest", "div_game", "depth_team", "total_implicito_equipo",
    "equipo_receiving_yards_season_avg", "equipo_receiving_yards_last3_avg",
    "equipo_receiving_yards_last5_avg", "equipo_receiving_yards_career_avg",
    "equipo_targets_season_avg", "equipo_targets_last3_avg",
    "equipo_targets_last5_avg", "equipo_targets_career_avg",
    "cambio_qb_titular_num", "cambio_equipo_num",
    "target_share_x_ofensiva_equipo", "draft_pick_x_experiencia", "cambio_qb_x_target_share",
]
"""Candidatas numericas fuera de las ventanas: volatilidad, perfil del jugador, contexto de
partido y de equipo, cambios de QB o de equipo, interacciones y el total implicito."""

CANDIDATAS_CATEGORICAS = ["roof", "surface", "anios_experiencia_bucket"]

VARIABLES_SELECCION_3_2 = [
    "air_yards_share_career_avg",
    "air_yards_share_last3_avg",
    "air_yards_share_season_avg",
    "anios_experiencia",
    "catch_rate_career_avg",
    "depth_team",
    "draft_pick",
    "edad",
    "fantasy_points_ppr_last3_avg",
    "fantasy_points_ppr_last5_avg",
    "fantasy_points_ppr_season_avg",
    "receiving_10_last3_avg",
    "receiving_16_last5_avg",
    "receiving_20_last3_avg",
    "receiving_air_yards_season_avg",
    "receiving_epa_last3_avg",
    "receiving_first_downs_career_avg",
    "receiving_first_downs_last5_avg",
    "receiving_tds_season_avg",
    "receiving_yards_career_avg",
    "receiving_yards_last3_avg",
    "receiving_yards_season_avg",
    "receptions_career_avg",
    "receptions_last3_avg",
    "receptions_last5_avg",
    "receptions_season_avg",
    "target_share_career_avg",
    "target_share_last3_avg",
    "target_share_last5_avg",
    "targets_last3_avg",
    "targets_last5_avg",
    "targets_season_avg",
    "wopr_last3_avg",
    "wopr_last5_avg",
]
"""Las 34 variables que eligio 3.2_seleccion_features.ipynb (union de las 15 mas importantes de
cada target, mas edad y experiencia). Quedan como registro: 3.3_seleccion_estable.ipynb las usa de
referencia al comparar conjuntos."""


def columnas_candidatas():
    """(numericas, categoricas) de la seleccion de variables: 84 en ventanas, 23 de contexto
    y 3 categoricas -- las 109 de 3.2_seleccion_features.ipynb mas el total implicito."""
    numericas = [f"{c}{v}" for c in COLUMNAS_BASE_CANDIDATAS for v in VENTANAS_CANDIDATAS]
    return numericas + list(CANDIDATAS_CONTEXTO), list(CANDIDATAS_CATEGORICAS)


def construir_tabla_candidatas(seasons=range(2016, 2026)):
    """Tabla WR por jugador-semana con todas las candidatas (columnas_candidatas()). Misma
    construccion que 3.2_seleccion_features.ipynb, con dos cambios: la alineacion es la
    unificada (data.cargar_depth_charts_unificado, con dato desde 2025, la que usan los
    modelos) y se agrega el total implicito. Todas se conocen antes del partido: las
    estadisticas entran rezagadas y el contexto (linea, descanso, estadio) se publica antes.

    Requiere `data.py` en el mismo path (sys.path.insert(0, "../src"))."""
    import data

    seasons = list(seasons)
    stats = data.cargar_stats_semanales(seasons)
    jugadores = data.cargar_jugadores()
    calendario = data.cargar_calendario(seasons)
    depth = data.cargar_depth_charts_unificado(seasons, calendario=calendario, posicion=features.POSICION)
    qb_titular = data.identificar_qb_titular(stats)

    wr = stats[stats["position"] == features.POSICION].copy()
    wr = features.calcular_ratios_eficiencia(wr)
    wr["racr"] = (wr["receiving_yards"] / wr["receiving_air_yards"]).where(wr["receiving_air_yards"] > 0)
    wr = acotar_variable(wr, "racr", percentil=0.99)
    wr = features.agregar_promedios_jugador(wr, COLUMNAS_BASE_CANDIDATAS, ventanas=(3, 5))
    wr = agregar_volatilidad_jugador(wr, ["receiving_yards", "targets"], ventana=5)
    wr = features.agregar_edad_experiencia(wr, jugadores)
    wr = features.agregar_experiencia_no_lineal(wr)
    wr = features.agregar_draft_pick(wr, jugadores)
    wr = agregar_ofensiva_equipo(wr, columnas=("receiving_yards", "targets"), ventanas=(3, 5))
    wr = agregar_cambio_qb_titular(wr, qb_titular)
    wr["cambio_qb_titular_num"] = wr["cambio_qb_titular"].astype(float)
    wr["cambio_equipo_num"] = wr["cambio_equipo"].astype(float)
    wr = agregar_total_implicito(wr, calendario)

    partido = ["season", "week", "div_game", "roof", "surface"]
    local = calendario[partido + ["home_team", "home_rest"]].rename(columns={"home_team": "team", "home_rest": "rest"})
    visita = calendario[partido + ["away_team", "away_rest"]].rename(columns={"away_team": "team", "away_rest": "rest"})
    filas = len(wr)
    wr = wr.merge(pd.concat([local, visita], ignore_index=True), on=["season", "week", "team"], how="left")
    wr = wr.merge(depth[["season", "week", "team", "player_id", "depth_team"]],
                  on=["season", "week", "team", "player_id"], how="left")
    assert len(wr) == filas, "los cruces con calendario y alineaciones no deben duplicar filas"

    wr = agregar_interaccion(wr, "target_share_last5_avg", "equipo_receiving_yards_season_avg",
                             "target_share_x_ofensiva_equipo")
    wr = agregar_interaccion(wr, "draft_pick", "anios_experiencia", "draft_pick_x_experiencia")
    wr = agregar_interaccion(wr, "cambio_qb_titular_num", "target_share_last5_avg", "cambio_qb_x_target_share")
    return wr.reset_index(drop=True)
