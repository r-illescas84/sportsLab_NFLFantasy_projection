"""
Aplica los modelos ya evaluados: ultimo paso del flujo semanal (pipeline.py).

Cada modelo es un archivo en weekly/wr/models/ ({target}_xgboost.json) con su metadata
({target}_metadata.json): las variables en el orden del entrenamiento, los hiperparametros, la
media del target en entrenamiento y las metricas de referencia de su evaluacion. Aqui no se
entrena ni se divide nada: decidir y evaluar modelos se hace en los notebooks con experimentos.py,
que tambien deja el archivo guardado en este formato (experimentos.guardar_modelo).

Metricas (docs/decisions/0004-metricas-de-seleccion.md): los modelos predicen valores esperados,
asi que la metrica principal tiene que premiar la media y no la mediana (Gneiting 2011): RMSE para
recepciones y yardas, deviance de Poisson para touchdowns, un conteo con ~82% de ceros donde el MAE
premia predecir cero. R2 y D2 se miden fuera de muestra, contra la media de entrenamiento
(Campbell y Thompson 2008): un valor negativo significa peor que el promedio historico.

Se carga con xgboost.Booster y no con el wrapper XGBRegressor: con las versiones fijadas en
docker/requirements.txt el wrapper falla al recargar un modelo guardado (docs/HALLAZGOS.md).
"""
import json
from pathlib import Path

import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.metrics import (
    brier_score_loss,
    mean_absolute_error,
    mean_poisson_deviance,
    mean_squared_error,
    r2_score,
    roc_auc_score,
)

TARGETS = ("receptions", "receiving_yards", "receiving_tds")

POLITICA_METRICAS = {
    "receptions": {"principal": "rmse", "conteo": True, "evento": False},
    "receiving_yards": {"principal": "rmse", "conteo": False, "evento": False},
    "receiving_tds": {"principal": "deviance_poisson", "conteo": True, "evento": True},
}
"""Por target: la metrica con la que se elige el modelo (`principal`), si se reportan las
metricas de conteo (deviance de Poisson y D2) y si se reportan las de evento ("anota al menos
uno": Brier y AUC)."""

METRICA_PRINCIPAL = {target: politica["principal"] for target, politica in POLITICA_METRICAS.items()}

METRICAS_MAYOR_ES_MEJOR = {"r2", "r2_oos", "d2_oos", "auc_anota"}


def _pendiente(y_true, y_pred):
    """Pendiente de la regresion de lo real sobre lo predicho: 1 si las predicciones estan en la
    escala correcta, menor a 1 si son demasiado extremas, mayor a 1 si son demasiado timidas."""
    varianza = np.var(y_pred)
    if varianza == 0:
        return np.nan
    return float(np.cov(y_pred, y_true, bias=True)[0, 1] / varianza)


def metricas(y_true, y_pred, media_entrenamiento=None, conteo=False, evento=False):
    """Metricas de una prediccion de valor esperado.

    Siempre: rmse, mae, r2 (contra la media del propio conjunto evaluado), sesgo (promedio
    predicho menos promedio real), media_real y pendiente_calibracion. Con `media_entrenamiento`:
    r2_oos, contra la media de entrenamiento, y sesgo_media_entrenamiento (el sesgo que tendria
    predecir esa media: el cambio de nivel entre temporadas). Con `conteo`: deviance_poisson y, con
    `media_entrenamiento`, d2_oos (fraccion de deviance explicada contra esa media). Con `evento`:
    brier_anota y auc_anota para "al menos uno", con P = 1 - exp(-prediccion)."""
    y_true = np.asarray(y_true, dtype=float)
    y_pred = np.asarray(y_pred, dtype=float)
    mse = mean_squared_error(y_true, y_pred)
    salida = {
        "rmse": mse ** 0.5,
        "mae": mean_absolute_error(y_true, y_pred),
        "r2": r2_score(y_true, y_pred),
    }
    if media_entrenamiento is not None:
        referencia = np.full_like(y_true, media_entrenamiento)
        salida["r2_oos"] = 1 - mse / mean_squared_error(y_true, referencia)
    salida["sesgo"] = float(y_pred.mean() - y_true.mean())
    if media_entrenamiento is not None:
        salida["sesgo_media_entrenamiento"] = float(media_entrenamiento - y_true.mean())
    salida["media_real"] = float(y_true.mean())
    salida["pendiente_calibracion"] = _pendiente(y_true, y_pred)
    if conteo:
        tasa = np.clip(y_pred, 1e-6, None)
        salida["deviance_poisson"] = mean_poisson_deviance(y_true, tasa)
        if media_entrenamiento is not None:
            salida["d2_oos"] = 1 - salida["deviance_poisson"] / mean_poisson_deviance(
                y_true, np.full_like(y_true, media_entrenamiento)
            )
        if evento:
            anota = y_true > 0
            salida["brier_anota"] = brier_score_loss(anota, 1 - np.exp(-tasa))
            salida["auc_anota"] = roc_auc_score(anota, tasa) if 0 < anota.mean() < 1 else np.nan
    return salida


def metricas_target(target, y_true, y_pred, media_entrenamiento=None):
    """metricas() con las opciones de POLITICA_METRICAS para `target`."""
    politica = POLITICA_METRICAS[target]
    return metricas(y_true, y_pred, media_entrenamiento, conteo=politica["conteo"], evento=politica["evento"])


def clip_no_negativo(pred):
    """Ningun target (recepciones/yardas/TDs) puede ser negativo en la
    realidad -- esto corrige cualquier prediccion por debajo de cero."""
    return np.clip(pred, 0, None)


def cargar_modelos(carpeta, targets=TARGETS):
    """Lee el modelo y la metadata de cada target. Regresa
    {target: {"booster", "variables", "media_entrenamiento", "metrica_principal",
    "referencia", "limites_seguimiento", "entrenado_con", "fecha_de_guardado"}}.
    `limites_seguimiento` es None en un modelo guardado sin ellos."""
    carpeta = Path(carpeta)
    modelos = {}
    for target in targets:
        booster = xgb.Booster()
        booster.load_model(str(carpeta / f"{target}_xgboost.json"))
        metadata = json.loads((carpeta / f"{target}_metadata.json").read_text())
        modelos[target] = {
            "booster": booster,
            "variables": metadata["features_arbol_en_orden"],
            "media_entrenamiento": metadata.get("media_entrenamiento"),
            "metrica_principal": metadata.get("metrica_principal", METRICA_PRINCIPAL[target]),
            "referencia": metadata["metricas_referencia"],
            "limites_seguimiento": metadata.get("limites_seguimiento"),
            "entrenado_con": metadata["entrenado_con"],
            "fecha_de_guardado": metadata.get("fecha_de_guardado"),
        }
    return modelos


def predecir(tabla, modelos):
    """Aplica cada modelo a `tabla` (salida de features.construir_tabla_modelado,
    filtrada a las filas a predecir). Cada modelo toma sus propias variables, en el
    orden con el que se entreno. Regresa una prediccion por jugador-semana y
    target, sin valores negativos."""
    salida = tabla[["player_id", "player_display_name", "team", "season", "week"]].copy()
    for target, modelo in modelos.items():
        matriz = xgb.DMatrix(tabla[modelo["variables"]])
        salida[f"{target}_pred"] = clip_no_negativo(modelo["booster"].predict(matriz)).astype("float64")
    return salida


def evaluar_predicciones(predicciones, reales, modelos):
    """Compara lo predicho contra lo que paso, para una semana ya jugada. `reales` es la tabla
    con los targets reales (mismo indice que `predicciones`). Junto a las metricas de la semana
    va la metrica principal de cada modelo y su valor de referencia en validacion y en prueba,
    para ver de un vistazo si la semana se parece a lo esperado."""
    filas = {}
    for target, modelo in modelos.items():
        real = reales[target]
        con_real = real.notna()
        principal = modelo["metrica_principal"]
        referencia = modelo["referencia"]
        filas[target] = {
            "n": int(con_real.sum()),
            "metrica_principal": principal,
            **metricas_target(
                target, real[con_real], predicciones.loc[con_real, f"{target}_pred"],
                media_entrenamiento=modelo["media_entrenamiento"],
            ),
            "referencia_validacion": referencia.get("validacion", {}).get(principal),
            "referencia_prueba": referencia.get("prueba", {}).get(principal),
        }
    return pd.DataFrame(filas).T.infer_objects()
