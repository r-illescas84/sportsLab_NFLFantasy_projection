"""
Aplica los modelos ya evaluados: ultimo paso del flujo semanal (pipeline.py).

Cada modelo es un archivo en weekly/wr/models/ ({target}_xgboost.json) con su metadata
({target}_metadata.json): las variables en el orden del entrenamiento, los hiperparametros
y las metricas de referencia de cuando se evaluo. Aqui no se entrena ni se divide nada:
decidir y evaluar modelos se hace en los notebooks con experimentos.py, que tambien deja el
archivo guardado en este formato (experimentos.guardar_modelo).

Se carga con xgboost.Booster y no con el wrapper XGBRegressor: con las versiones fijadas en
docker/requirements.txt el wrapper falla al recargar un modelo guardado (docs/HALLAZGOS.md).
"""
import json
from pathlib import Path

import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score

TARGETS = ("receptions", "receiving_yards", "receiving_tds")


def metricas(y_true, y_pred):
    """MAE/RMSE/R2 -- las mismas 3 que ya reporta preeliminar/03_modelo_predictivo
    y el pipeline heredado, para poder comparar directamente contra ambos."""
    return {
        "mae": mean_absolute_error(y_true, y_pred),
        "rmse": mean_squared_error(y_true, y_pred) ** 0.5,
        "r2": r2_score(y_true, y_pred),
    }


def clip_no_negativo(pred):
    """Ningun target (recepciones/yardas/TDs) puede ser negativo en la
    realidad -- esto corrige cualquier prediccion por debajo de cero."""
    return np.clip(pred, 0, None)


def cargar_modelos(carpeta, targets=TARGETS):
    """Lee el modelo y la metadata de cada target. Regresa
    {target: {"booster", "variables", "referencia", "entrenado_con"}}."""
    carpeta = Path(carpeta)
    modelos = {}
    for target in targets:
        booster = xgb.Booster()
        booster.load_model(str(carpeta / f"{target}_xgboost.json"))
        metadata = json.loads((carpeta / f"{target}_metadata.json").read_text())
        modelos[target] = {
            "booster": booster,
            "variables": metadata["features_arbol_en_orden"],
            "referencia": metadata["metricas_referencia"],
            "entrenado_con": metadata["entrenado_con"],
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
    """Compara lo predicho contra lo que paso, para una semana ya jugada. `reales`
    es la tabla con los targets reales (mismo indice que `predicciones`). Junto a
    las metricas de la semana va el MAE de referencia del modelo (el de su
    evaluacion en test), para ver de un vistazo si la semana se parece a lo
    esperado."""
    filas = {}
    for target, modelo in modelos.items():
        real = reales[target]
        con_real = real.notna()
        filas[target] = {
            "n": int(con_real.sum()),
            **metricas(real[con_real], predicciones.loc[con_real, f"{target}_pred"]),
            "mae_referencia": modelo["referencia"]["mae_test_2024_2025"],
        }
    return pd.DataFrame(filas).T
