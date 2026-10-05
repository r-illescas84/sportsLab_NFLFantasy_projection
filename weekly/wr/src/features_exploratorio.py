"""
Variables de la Fase 4 que se construyeron y evaluaron pero NO entraron al modelo
(tabla de decisiones en features.py; evidencia en notebooks/10 y 11).

Solo se usan desde los notebooks. El flujo semanal (pipeline.py) no las calcula: el
modelo vigente solo usa las variables de features.FEATURES_SELECCIONADAS_ARBOL.
"""
import features


def acotar_variable(stats, columna, limite=None, percentil=0.99):
    """Topa una columna inestable sobre su valor semanal CRUDO, antes de
    promediar -- evidencia: racr promediado empeora su curtosis (241 crudo -> 605
    promediado, ver 05_eda_perfil_variables.ipynb), un solo valor disparado
    arrastra el promedio de todo un jugador. limite=None calcula el percentil
    real de la columna en vez de un numero inventado."""
    df = stats.copy()
    tope = df[columna].quantile(percentil) if limite is None else limite
    df[f"{columna}_acotado"] = df[columna].clip(upper=tope)
    return df


def agregar_volatilidad_jugador(stats, columnas, ventana=5, id_col="player_id"):
    """Desviacion estandar de las ultimas `ventana` apariciones del jugador --
    mide consistencia/boom-bust, no solo nivel promedio (conectado con el
    hallazgo de 09_eda_jugador_destacado.ipynb sobre semanas boom). shift(1)
    antes de rolling().std(), mismo patron leak-safe que
    agregar_promedios_jugador(). min_periods=2: un desvio con una sola
    observacion no esta definido."""
    df = stats.sort_values([id_col, "season", "week"]).copy()
    for col in columnas:
        df[f"{col}_volatilidad{ventana}"] = df.groupby(id_col)[col].transform(
            lambda s: s.shift(1).rolling(ventana, min_periods=2).std()
        )
    return df


def agregar_ofensiva_equipo(stats, columnas=("receiving_yards",), ventanas=(3, 5), team_col="team"):
    """Agregado de equipo-semana (suma de todos los WR de ese equipo esa semana),
    rezagado reutilizando agregar_promedios_jugador() con id_col=team_col -- no
    duplica la logica de shift/rolling ya verificada sin fuga.

    OJO con la expectativa correcta: 0.40 de estabilidad (07_eda_equipos.ipynb)
    es ANUAL, equipo-temporada. La version semana a semana ya se probo en
    08_eda_multivariable.ipynb y da ~0.08 de correlacion individual (aclarado en
    07, celda 11) -- se construye porque tiene una base real, no porque vaya a
    repetir el 0.40."""
    equipo_semana = stats.groupby(["season", team_col, "week"], as_index=False)[
        list(columnas)
    ].sum()
    equipo_semana = features.agregar_promedios_jugador(
        equipo_semana, columnas, ventanas=ventanas, id_col=team_col
    )
    columnas_nuevas = [
        c
        for c in equipo_semana.columns
        if c not in ["season", team_col, "week"] + list(columnas)
    ]
    equipo_semana = equipo_semana.rename(
        columns={c: f"equipo_{c}" for c in columnas_nuevas}
    )
    columnas_equipo = [f"equipo_{c}" for c in columnas_nuevas]
    return stats.merge(
        equipo_semana[["season", team_col, "week"] + columnas_equipo],
        on=["season", team_col, "week"],
        how="left",
    )


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


def agregar_interaccion(stats, col_a, col_b, nombre=None):
    """Feature de interaccion: producto de 2 columnas ya existentes (idealmente
    ya rezagadas). Generico para las combinaciones con razon concreta probadas en
    la Fase 4 (volumen x contexto de equipo, draft x experiencia, cambio de QB x
    volumen) -- no se escribe una funcion distinta por cada par."""
    df = stats.copy()
    nombre = nombre or f"{col_a}_x_{col_b}"
    df[nombre] = df[col_a] * df[col_b]
    return df
