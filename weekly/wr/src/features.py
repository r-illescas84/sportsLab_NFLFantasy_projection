"""
Feature engineering para el pipeline de WR semanal.

Reemplaza la logica que hoy vive triplicada (una copia casi identica en cada
uno de wrs_rec_tds.ipynb, wrs_rec_yds.ipynb, wrs_receptions.ipynb, ver
weekly/wr/notebooks/_reference/) por un solo lugar.

target_share, air_yards_share y wopr NO se calculan aqui -- ya vienen nativos
y limpios en cargar_stats_semanales() (0% nulo para WR, ver DATASHEET.md). El
pipeline heredado los recalculaba a mano desde play-by-play dos veces por
notebook; con nflreadpy eso ya no hace falta.

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
| depth_team | Su lugar en la alineacion del equipo (1=titular/WR1, 2=WR2...) -- mas bajo es mejor | Incluir | Señal fuerte y nueva en recepciones/yardas (nb 11); 0% cobertura en 2025+ hasta unificar esquemas |
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
    equipo_semana = agregar_promedios_jugador(
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


def columnas_por_modelo(tipo="ridge"):
    """Evita la colinealidad 0.82-0.97 de target_share/air_yards_share/wopr
    (08_eda_multivariable.ipynb, wopr es combinacion ponderada exacta de las
    otras 2). 'ridge': solo wopr (ya las combina) + receiving_epa (la mas
    independiente del grupo, 0.35-0.48 de correlacion con las demas). 'arbol':
    las 4 originales -- la redundancia no perjudica a RandomForest/XGBoost."""
    if tipo == "ridge":
        return ["wopr", "receiving_epa"]
    if tipo == "arbol":
        return ["target_share", "air_yards_share", "wopr", "receiving_epa"]
    raise ValueError(f"tipo desconocido: {tipo!r}")
