"""
Seguimiento del flujo semanal de WR (docs/decisions/0006-seguimiento-semanal.md). Corre cada
semana desde pipeline.py:

1. chequear_calidad()  revisa la tabla de la semana en las seis dimensiones de
                       docs/data/DATASHEET.md antes de predecir o de evaluar. Lo que invalida la
                       prediccion detiene la corrida (detener_si_bloquea); lo demas se anota como
                       advertencia.
2. huella()            identifica el archivo de variables con que se predijo, para poder reproducir
                       la prediccion despues con el modelo guardado.
3. metricas_ventana()  mide las ultimas VENTANA_SEMANAS semanas jugadas de la temporada, y
   evaluar_alertas()   las compara contra los limites de control guardados con cada modelo
                       (experimentos.limites_de_seguimiento, en 6.1_modelo_final_y_temporada_actual).
                       Una alerta persistente (fuera tambien en la ventana anterior sin semanas en
                       comun) pide revisar el modelo; el flujo nunca reentrena solo.
4. registrar_corrida() agrega las filas de la corrida a outputs/tracking.csv, el historial de todas
                       las corridas: solo se agregan filas, nunca se reescriben.
"""
import hashlib
from pathlib import Path

import numpy as np
import pandas as pd

import modeling

BLOQUEA = "bloquea"
ADVIERTE = "advierte"

UMBRAL_VACIOS = 0.10
"""Una variable con mas vacios que su historico de la misma semana, por mas de 10 puntos, se
advierte."""

VENTANA_SEMANAS = 4

METRICAS_PRINCIPALES = set(modeling.METRICA_PRINCIPAL.values())

COLUMNAS_ID = ["player_id", "player_display_name", "team", "season", "week"]

COLUMNAS_TRACKING = [
    "fecha_corrida", "season", "week", "estado", "target", "origen_prediccion", "n_wr",
    "n_predichos_sin_real", "n_reales_sin_prediccion", "modelo_entrenado_con", "modelo_fecha_guardado",
    "huella_variables", "metrica_principal", "valor", "referencia_validacion", "referencia_prueba", "r2_oos",
    "sesgo", "pendiente_calibracion", "ventana", "ventana_valor", "ventana_sesgo", "ventana_pendiente",
    "alertas", "advertencias",
]
"""Una fila por corrida y target. En una semana pendiente las columnas de resultado quedan vacias;
`origen_prediccion` es 'emitida' si se evaluo la prediccion guardada antes del partido y
'reconstruida' si no existia y se recalculo con los modelos vigentes."""


class ChequeoBloqueante(ValueError):
    """Un chequeo de calidad invalida la prediccion: la corrida se detiene sin escribir nada."""


def _resultado(dimension, chequeo, nivel, fallas, detalle=""):
    return {"dimension": dimension, "chequeo": chequeo, "nivel": nivel, "ok": not fallas, "detalle": detalle}


def chequear_calidad(semana, historico, variables, estado, juegos):
    """Chequeos de la tabla de una semana, por dimension de calidad (docs/data/DATASHEET.md).

    `semana`: filas de la semana (las de la alineacion si esta pendiente, las reales si ya se
    jugo); `historico`: filas reales anteriores a la semana; `variables`: las de los modelos;
    `estado`: 'pendiente' o 'jugada'; `juegos`: partidos de la semana en el calendario. La semana
    en curso se revisa antes, en pipeline.estado_de_la_semana. Regresa un DataFrame con una fila
    por chequeo: dimension, chequeo, nivel (bloquea / advierte), ok y detalle."""
    season, week = int(juegos["season"].iloc[0]), int(juegos["week"].iloc[0])
    pendiente = estado == "pendiente"
    salida = []

    repetidos = semana["player_id"][semana["player_id"].duplicated()].unique()
    salida.append(_resultado("Unicidad", "una fila por jugador", BLOQUEA, len(repetidos),
                             f"{len(repetidos)} jugadores repetidos: {', '.join(repetidos[:5])}" if len(repetidos) else ""))

    ausentes = [v for v in variables if v not in semana.columns]
    salida.append(_resultado("Completitud", "variables de los modelos presentes", BLOQUEA, ausentes,
                             ", ".join(ausentes)))
    presentes = [v for v in variables if v in semana.columns]
    vacios = semana[presentes].isna().mean()
    vacios_hist = historico.loc[historico["week"] == week, presentes].isna().mean()
    todo_vacio = [v for v in presentes if vacios[v] == 1 and vacios_hist[v] < 1]
    salida.append(_resultado("Completitud", "ninguna variable 100% vacia", BLOQUEA, todo_vacio,
                             ", ".join(todo_vacio)))
    exceso = (vacios - vacios_hist)[lambda d: d > UMBRAL_VACIOS]
    salida.append(_resultado(
        "Completitud", f"vacios a no mas de {UMBRAL_VACIOS * 100:.0f} puntos sobre el historico de la semana {week}", ADVIERTE,
        len(exceso), ", ".join(f"{v} {vacios[v]:.1%} (historico {vacios_hist[v]:.1%})" for v in exceso.index)))

    equipos = set(juegos["home_team"]) | set(juegos["away_team"])
    ajenos = sorted(set(semana["team"]) - equipos)
    salida.append(_resultado("Consistencia", "equipos de la tabla en el calendario de la semana", ADVIERTE, ajenos,
                             ", ".join(ajenos)))
    sin_filas = sorted(equipos - set(semana["team"]))
    salida.append(_resultado(
        "Consistencia", "cada equipo que juega tiene receptores", BLOQUEA if pendiente else ADVIERTE, sin_filas,
        f"faltan {len(sin_filas)} de {len(equipos)} equipos: {', '.join(sin_filas)}"
        + (". Correr cuando ya haya terminado la semana anterior" if pendiente else "") if sin_filas else ""))

    minimos, maximos = historico[presentes].min(), historico[presentes].max()
    fuera = {v: int(((semana[v] < minimos[v]) | (semana[v] > maximos[v])).sum()) for v in presentes}
    fuera = {v: n for v, n in fuera.items() if n}
    salida.append(_resultado("Validez", "valores dentro del rango historico", ADVIERTE, fuera,
                             ", ".join(f"{v} ({n} filas)" for v, n in fuera.items())))

    if pendiente:
        cargadas = historico.loc[historico["season"] == season, "week"]
        ultima = int(cargadas.max()) if len(cargadas) else 0
        salida.append(_resultado("Vigencia", "estadisticas de la semana anterior cargadas", BLOQUEA,
                                 week > 1 and ultima != week - 1,
                                 f"ultima semana con estadisticas: {ultima}" if week > 1 and ultima != week - 1 else ""))
        sin_lineas = juegos[juegos["spread_line"].isna() | juegos["total_line"].isna()]
        salida.append(_resultado("Vigencia", "lineas de apuestas en cada partido", ADVIERTE, len(sin_lineas),
                                 ", ".join(sin_lineas["away_team"] + "@" + sin_lineas["home_team"])))
    else:
        salida.append(_resultado("Vigencia", "estadisticas reales de la semana cargadas", BLOQUEA, semana.empty))
        ppr = (semana["fantasy_points_ppr"] - semana["fantasy_points"] - semana["receptions"]).abs() > 0.01
        salida.append(_resultado("Exactitud", "fantasy_points_ppr = fantasy_points + receptions", ADVIERTE,
                                 int(ppr.sum()), f"{int(ppr.sum())} filas" if ppr.any() else ""))
        imposibles = semana["receptions"] > semana["targets"]
        salida.append(_resultado("Exactitud", "recepciones <= pases dirigidos", ADVIERTE, int(imposibles.sum()),
                                 f"{int(imposibles.sum())} filas" if imposibles.any() else ""))
    return pd.DataFrame(salida)


def resultado_vigencia(chequeo, fallas, detalle=""):
    """Chequeo de vigencia que se decide fuera de chequear_calidad() (p. ej. si existe la
    prediccion emitida), en el mismo formato."""
    return pd.DataFrame([_resultado("Vigencia", chequeo, ADVIERTE, fallas, detalle)])


def detener_si_bloquea(chequeos):
    """Levanta ChequeoBloqueante con todos los chequeos bloqueantes que fallaron."""
    fallidos = chequeos[(chequeos["nivel"] == BLOQUEA) & ~chequeos["ok"]]
    if not fallidos.empty:
        lineas = [f"- {f.dimension}: {f.chequeo}" + (f" ({f.detalle})" if f.detalle else "") for f in fallidos.itertuples()]
        raise ChequeoBloqueante("la corrida se detiene, chequeos de calidad que invalidan la prediccion:\n"
                                + "\n".join(lineas))


def advertencias(chequeos):
    """Texto de los chequeos de advertencia que fallaron, para la columna del tracking."""
    fallidos = chequeos[(chequeos["nivel"] == ADVIERTE) & ~chequeos["ok"]]
    return "; ".join(f"{f.chequeo}: {f.detalle}" if f.detalle else f.chequeo for f in fallidos.itertuples())


def huella(ruta):
    """Primeros 16 caracteres del SHA-256 del archivo: cambia si cambia cualquier valor."""
    return hashlib.sha256(Path(ruta).read_bytes()).hexdigest()[:16]


def prediccion_emitida(carpeta, week):
    """La prediccion guardada antes del partido, o None si no existe. Solo cuenta si tambien
    existe su archivo de variables: los dos se escriben juntos al predecir una semana pendiente."""
    ruta, variables = Path(carpeta) / f"predicciones_semana_{week}.csv", Path(carpeta) / f"variables_semana_{week}.csv"
    if not (ruta.exists() and variables.exists()):
        return None
    return pd.read_csv(ruta)


def metricas_ventana(evaluaciones, modelos):
    """Metricas de la ventana (evaluaciones de varias semanas juntas, con columnas {target}_pred y
    {target}_real): la metrica principal, el sesgo y la pendiente de cada target."""
    salida = {}
    for target, modelo in modelos.items():
        principal = modelo["metrica_principal"]
        con_real = evaluaciones[f"{target}_real"].notna()
        m = modeling.metricas_target(target, evaluaciones.loc[con_real, f"{target}_real"],
                                     evaluaciones.loc[con_real, f"{target}_pred"])
        salida[target] = {principal: m[principal], "sesgo": m["sesgo"],
                          "pendiente_calibracion": m["pendiente_calibracion"]}
    return salida


def evaluar_alertas(valores, limites, alertas_previas=""):
    """Compara las metricas de una ventana contra los limites de control de un target. La metrica
    principal alerta solo si queda por encima (un error menor no es problema); el sesgo y la
    pendiente, de cualquier lado. Una alerta es persistente si la misma metrica ya estaba fuera en
    la ventana que termina VENTANA_SEMANAS semanas antes, la primera sin semanas en comun con esta
    (`alertas_previas`, el texto de su fila en el tracking): dos ventanas seguidas comparten tres
    de sus cuatro semanas y casi no agregan evidencia. Regresa el texto de la columna `alertas`
    (vacio si no hay)."""
    alertas = []
    for metrica, valor in valores.items():
        inferior, superior = limites["metricas"][metrica]["inferior"], limites["metricas"][metrica]["superior"]
        if valor > superior:
            lado = f"{metrica} {valor:.4g} > {superior:.4g}"
        elif metrica not in METRICAS_PRINCIPALES and valor < inferior:
            lado = f"{metrica} {valor:.4g} < {inferior:.4g}"
        else:
            continue
        persistente = any(a.split(" ")[0] == metrica for a in str(alertas_previas).split("; ") if a)
        alertas.append(lado + (" (persistente)" if persistente else ""))
    return "; ".join(alertas)


def leer_tracking(ruta):
    """El historial completo, o una tabla vacia con las columnas del tracking si aun no existe."""
    ruta = Path(ruta)
    if not ruta.exists():
        return pd.DataFrame(columns=COLUMNAS_TRACKING)
    return pd.read_csv(ruta, keep_default_na=False, na_values=[""])


def ultimas_corridas(tracking):
    """La corrida mas reciente de cada temporada, semana, estado y target: la que se usa para
    tablas y alertas (las anteriores quedan como registro)."""
    return (tracking.sort_values("fecha_corrida")
            .drop_duplicates(["season", "week", "estado", "target"], keep="last")
            .sort_values(["season", "week", "estado", "target"]).reset_index(drop=True))


def registrar_corrida(filas, ruta):
    """Agrega las filas de una corrida al final del tracking (lo crea si no existe)."""
    ruta = Path(ruta)
    nuevas = pd.DataFrame(filas).reindex(columns=COLUMNAS_TRACKING)
    nuevas.to_csv(ruta, mode="a", header=not ruta.exists(), index=False)
    return nuevas
