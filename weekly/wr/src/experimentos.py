"""
Herramientas de analisis para decidir QUE modelo usar -- solo se llaman desde los
notebooks, el flujo semanal (pipeline.py) no las usa.

Aqui vive todo lo que ajusta, compara y valida modelos (split temporal, walk-forward,
comparacion de familias de modelos, busqueda de hiperparametros, modelo de 2 etapas) y
guardar_modelo(), que deja el archivo del modelo ya evaluado en el formato que lee
modeling.cargar_modelos(). Lo que se ejecuta cada semana (cargar, predecir, medir) esta
en modeling.py.

Split SIEMPRE temporal, nunca aleatorio -- ese fue exactamente el error del
pipeline heredado (weekly/wr/notebooks/_reference/), confirmado en su propia
celda de entrenamiento: train_test_split(test_size=0.1, random_state=42)
sobre semanas apiladas de cualquier temporada, con un split por temporada ya
escrito pero dejado comentado.
"""
import json
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.ensemble import (
    HistGradientBoostingClassifier,
    HistGradientBoostingRegressor,
    RandomForestRegressor,
)
from sklearn.impute import SimpleImputer
from sklearn.linear_model import Ridge
from sklearn.metrics import mean_absolute_error
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from xgboost import XGBRegressor

from modeling import metricas


def dividir_temporal(df, train_seasons, val_seasons, test_seasons, season_col="season"):
    """3 mascaras booleanas por temporada -- nunca aleatorio. Revienta si una
    temporada aparece en mas de una particion, para no repetir en silencio el
    error del pipeline heredado."""
    train_seasons, val_seasons, test_seasons = set(train_seasons), set(val_seasons), set(test_seasons)
    solapadas = (train_seasons & val_seasons) | (train_seasons & test_seasons) | (val_seasons & test_seasons)
    if solapadas:
        raise ValueError(f"temporadas repetidas entre particiones: {solapadas}")
    return (
        df[season_col].isin(train_seasons),
        df[season_col].isin(val_seasons),
        df[season_col].isin(test_seasons),
    )


def folds_walk_forward(df, anios_test, season_col="season"):
    """Un fold por anio en `anios_test`: entrena con todo lo anterior a ese
    anio, prueba en ese anio -- mismo patron que
    preeliminar/03_modelo_predictivo/06_estabilidad_en_el_tiempo.ipynb del
    propio usuario. Genera (anio, train_mask, test_mask)."""
    for anio in anios_test:
        yield anio, df[season_col] < anio, df[season_col] == anio


def evaluar_modelo(modelo, X, y, train_mask, val_mask, test_mask, ajustar=True):
    """Ajusta (si ajustar=True) sobre train y regresa prediccion + metricas en
    las 3 particiones -- pieza generica que reutilizan entrenar_y_evaluar() y
    cualquier modelo ad-hoc (ej. los de conteo para receiving_tds), sin
    duplicar la logica de evaluar en cada notebook."""
    if ajustar:
        modelo.fit(X[train_mask], y[train_mask])
    pred = pd.Series(modelo.predict(X), index=X.index)
    return {
        "modelo": modelo,
        "pred": pred,
        "train": metricas(y[train_mask], pred[train_mask]),
        "val": metricas(y[val_mask], pred[val_mask]),
        "test": metricas(y[test_mask], pred[test_mask]),
    }


class BaselinePromedioJugador:
    """Predice el propio promedio de temporada del jugador
    (`{target}_season_avg`), con fallback a `{target}_career_avg` y despues a
    la media de entrenamiento -- mismo baseline que
    preeliminar/03_modelo_predictivo/04_modelado_y_seleccion.ipynb del propio
    usuario, para comparar contra el mismo criterio."""

    def __init__(self, target):
        self.target = target

    def fit(self, X, y):
        self.media_train_ = y.mean()
        return self

    def predict(self, X):
        season = X[f"{self.target}_season_avg"]
        career = X[f"{self.target}_career_avg"]
        return season.fillna(career).fillna(self.media_train_).to_numpy()


def resumen_resultados(resultados):
    """De un dict {nombre: salida de evaluar_modelo()} arma un DataFrame
    MAE/RMSE/R2 por particion, ordenado por MAE de validacion -- mismo
    criterio de seleccion que preeliminar/03_modelo_predictivo (elegir por
    error de validacion, reportar test una sola vez)."""
    filas = {
        nombre: {
            f"{parte}_{metrica}": valor
            for parte in ("train", "val", "test")
            for metrica, valor in salida[parte].items()
        }
        for nombre, salida in resultados.items()
    }
    return pd.DataFrame(filas).T.sort_values("val_mae")


def entrenar_y_evaluar(df, target, features_arbol, features_ridge, train_mask, val_mask, test_mask, semilla=0):
    """Compara Baseline / Ridge / RandomForest / XGBoost / HistGradientBoosting
    para `target` -- el bake-off central de Fase 5. Ridge usa `features_ridge`
    (imputado + escalado -- corrige al pipeline heredado, que no escalaba);
    los arboles usan `features_arbol` sin escalar (XGBoost/HistGradientBoosting
    manejan NaN nativo, RandomForest se imputa aparte porque sklearn no
    acepta NaN ahi). Hiperparametros elegidos por una busqueda pequena
    validada en `val_mask` -- no fijos a ciegas (RandomForest en el pipeline
    heredado) ni sin comparar contra nada (ahi tampoco habia baseline).

    Regresa (resultados, resumen): `resultados` es un dict {nombre: salida de
    evaluar_modelo()}; `resumen` es la tabla de resumen_resultados()."""
    y = df[target]
    resultados = {}

    base = BaselinePromedioJugador(target).fit(df, y)
    resultados["Baseline"] = evaluar_modelo(base, df, y, train_mask, val_mask, test_mask, ajustar=False)

    X_ridge = pd.get_dummies(df[features_ridge], dummy_na=True)
    mejor_mae = np.inf
    for alpha in (0.1, 1.0, 10.0, 50.0):
        pipe = Pipeline([
            ("imputar", SimpleImputer(strategy="median")),
            ("escalar", StandardScaler()),
            ("modelo", Ridge(alpha=alpha)),
        ])
        salida = evaluar_modelo(pipe, X_ridge, y, train_mask, val_mask, test_mask)
        if salida["val"]["mae"] < mejor_mae:
            mejor_mae, resultados["Ridge"] = salida["val"]["mae"], salida

    X_arbol = df[features_arbol]
    X_arbol_imputado = pd.DataFrame(
        SimpleImputer(strategy="median").fit_transform(X_arbol),
        index=X_arbol.index,
        columns=X_arbol.columns,
    )
    mejor_mae = np.inf
    for max_depth in (6, 10, 14):
        for min_leaf in (3, 5, 10):
            modelo = RandomForestRegressor(
                n_estimators=300, max_depth=max_depth, min_samples_leaf=min_leaf,
                random_state=semilla, n_jobs=-1,
            )
            salida = evaluar_modelo(modelo, X_arbol_imputado, y, train_mask, val_mask, test_mask)
            if salida["val"]["mae"] < mejor_mae:
                mejor_mae, resultados["RandomForest"] = salida["val"]["mae"], salida

    mejor_mae = np.inf
    for max_depth in (3, 4, 6):
        for n_estimators in (200, 400):
            for lr in (0.03, 0.08):
                modelo = XGBRegressor(
                    max_depth=max_depth, n_estimators=n_estimators, learning_rate=lr,
                    random_state=semilla, n_jobs=-1,
                )
                salida = evaluar_modelo(modelo, X_arbol, y, train_mask, val_mask, test_mask)
                if salida["val"]["mae"] < mejor_mae:
                    mejor_mae, resultados["XGBoost"] = salida["val"]["mae"], salida

    mejor_mae = np.inf
    for max_iter in (100, 200, 300):
        for max_depth in (None, 6, 10):
            modelo = HistGradientBoostingRegressor(
                max_iter=max_iter, max_depth=max_depth, random_state=semilla
            )
            salida = evaluar_modelo(modelo, X_arbol, y, train_mask, val_mask, test_mask)
            if salida["val"]["mae"] < mejor_mae:
                mejor_mae, resultados["HistGradientBoosting"] = salida["val"]["mae"], salida

    return resultados, resumen_resultados(resultados)


ESPACIO_HIPERPARAMETROS_XGBOOST = {
    "max_depth": [3, 4, 5, 6, 7, 8, 9, 10],
    "learning_rate": [0.005, 0.01, 0.02, 0.03, 0.05, 0.08, 0.1, 0.15, 0.2],
    "subsample": [0.6, 0.7, 0.8, 0.9, 1.0],
    "colsample_bytree": [0.6, 0.7, 0.8, 0.9, 1.0],
    "min_child_weight": [1, 2, 3, 5, 7, 10],
    "reg_alpha": [0, 0.01, 0.1, 0.5, 1.0],
    "reg_lambda": [0.5, 1.0, 1.5, 2.0, 3.0, 5.0],
    "gamma": [0, 0.01, 0.05, 0.1, 0.3],
}
"""Espacio de busqueda mas amplio que la grilla pequena de entrenar_y_evaluar()
(que solo prueba max_depth/n_estimators/learning_rate, 12 combinaciones) --
agrega regularizacion (L1/L2/gamma) y submuestreo de filas/columnas, los
hiperparametros que de verdad controlan sobreajuste en XGBoost."""


def buscar_hiperparametros(df, target, features, train_mask, val_mask, n_intentos=100,
                            semilla=0, espacio=None, params_fijos=None):
    """Busqueda ALEATORIA de hiperparametros de XGBoost sobre `espacio`
    (por defecto ESPACIO_HIPERPARAMETROS_XGBOOST). `n_estimators` no se
    busca como valor fijo de una grilla: se pone un maximo generoso (2000) y
    se usa early stopping sobre `val_mask` (30 rondas sin mejorar) para que
    cada combinacion encuentre su propio numero optimo de arboles -- mas
    eficiente y mas correcto que probar valores de n_estimators a ciegas.

    `params_fijos`: dict de hiperparametros que NO se buscan (ej.
    `{"objective": "reg:tweedie", "tweedie_variance_power": 1.5}` para
    receiving_tds) -- se pasan tal cual a cada intento.

    Regresa (mejor_modelo, mejor_config, tabla_intentos): `mejor_modelo` ya
    esta ajustado; `tabla_intentos` tiene un renglon por combinacion
    probada -- util para ver que tan sensible es el MAE a cada
    hiperparametro, no solo cual gano."""
    from xgboost import XGBRegressor

    espacio = espacio or ESPACIO_HIPERPARAMETROS_XGBOOST
    rng = np.random.RandomState(semilla)
    X, y = df[features], df[target]

    intentos = []
    mejor_mae, mejor_modelo, mejor_config = np.inf, None, None
    for _ in range(n_intentos):
        config = {k: rng.choice(v).item() if hasattr(rng.choice(v), "item") else rng.choice(v)
                  for k, v in espacio.items()}
        params = {**config, **(params_fijos or {}), "n_estimators": 2000,
                  "random_state": semilla, "n_jobs": -1}
        modelo = XGBRegressor(**params, early_stopping_rounds=30, eval_metric="mae")
        modelo.fit(X[train_mask], y[train_mask], eval_set=[(X[val_mask], y[val_mask])], verbose=False)

        pred_val = modelo.predict(X[val_mask])
        mae_val = mean_absolute_error(y[val_mask], pred_val)
        intentos.append({**config, "n_estimators_optimo": modelo.best_iteration + 1, "val_mae": mae_val})

        if mae_val < mejor_mae:
            mejor_mae, mejor_modelo = mae_val, modelo
            mejor_config = {**params, "n_estimators": modelo.best_iteration + 1}

    tabla_intentos = pd.DataFrame(intentos).sort_values("val_mae").reset_index(drop=True)
    return mejor_modelo, mejor_config, tabla_intentos


def entrenar_hurdle(df, target, features_arbol, train_mask, val_mask, test_mask, semilla=0):
    """Modelo de 2 etapas para un target dominado por ceros (ej.
    receiving_tds, 82% en cero segun 04_eda_targets.ipynb): etapa 1 clasifica
    P(target > 0 | features) con HistGradientBoostingClassifier; etapa 2
    regresa el conteo esperado condicional a ser positivo
    (HistGradientBoostingRegressor(loss="poisson"), entrenado SOLO sobre
    filas con target > 0). Prediccion final = P(etapa 1) * E[etapa 2] -- la
    tecnica estandar para conteos con exceso de ceros que una regresion
    generica no separa. Regresa el mismo formato que evaluar_modelo()."""
    y = df[target]
    es_positivo = (y > 0).astype(int)
    X = df[features_arbol]

    clasificador = HistGradientBoostingClassifier(random_state=semilla)
    clasificador.fit(X[train_mask], es_positivo[train_mask])
    prob_positivo = pd.Series(clasificador.predict_proba(X)[:, 1], index=X.index)

    positivos_train = train_mask & (y > 0)
    regresor = HistGradientBoostingRegressor(loss="poisson", random_state=semilla)
    regresor.fit(X[positivos_train], y[positivos_train])
    conteo_condicional = pd.Series(regresor.predict(X), index=X.index)

    pred = prob_positivo * conteo_condicional
    return {
        "modelo": (clasificador, regresor),
        "pred": pred,
        "train": metricas(y[train_mask], pred[train_mask]),
        "val": metricas(y[val_mask], pred[val_mask]),
        "test": metricas(y[test_mask], pred[test_mask]),
    }


def guardar_modelo(modelo, carpeta, target, variables, metricas_referencia, entrenado_con, limitaciones=()):
    """Guarda un XGBRegressor ya evaluado: el archivo del modelo
    ({target}_xgboost.json, formato nativo de XGBoost) y su metadata
    ({target}_metadata.json: variables en el orden exacto del entrenamiento,
    hiperparametros, de que datos salio y las metricas de la evaluacion). Es lo
    unico que modeling.cargar_modelos() necesita para aplicarlo cada semana."""
    carpeta = Path(carpeta)
    carpeta.mkdir(parents=True, exist_ok=True)
    modelo.save_model(str(carpeta / f"{target}_xgboost.json"))
    metadata = {
        "target": target,
        "modelo": type(modelo).__name__,
        "hiperparametros": {k: v for k, v in modelo.get_params().items() if v is not None},
        "features_arbol_en_orden": list(variables),
        "entrenado_con": entrenado_con,
        "fecha_de_guardado": pd.Timestamp.now(tz="UTC").isoformat(),
        "metricas_referencia": metricas_referencia,
        "limitaciones_conocidas": list(limitaciones),
    }
    (carpeta / f"{target}_metadata.json").write_text(json.dumps(metadata, indent=2, ensure_ascii=False))
