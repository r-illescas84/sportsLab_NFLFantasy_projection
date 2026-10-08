"""
Herramientas de analisis para decidir QUE modelo usar -- solo se llaman desde los
notebooks, el flujo semanal (pipeline.py) no las usa.

Aqui vive todo lo que ajusta, compara y valida modelos (split temporal, walk-forward,
comparacion de familias de modelos, busqueda de hiperparametros, modelo de 2 etapas,
requisitos para que un modelo sea elegible) y guardar_modelo(), que deja el archivo del modelo
ya evaluado en el formato que lee modeling.cargar_modelos(). Lo que se ejecuta cada semana
(cargar, predecir, medir) esta en modeling.py.

Reglas de evaluacion (docs/decisions/0004-metricas-de-seleccion.md):
- Split SIEMPRE temporal, nunca aleatorio -- el pipeline original
  (weekly/wr/notebooks/_reference/) usaba train_test_split(test_size=0.1, random_state=42)
  sobre semanas apiladas de cualquier temporada.
- Se elige en validacion y la prueba solo se reporta, una vez.
- El criterio por defecto es la metrica principal de cada target
  (modeling.METRICA_PRINCIPAL): RMSE en recepciones y yardas, deviance de Poisson en touchdowns.
"""
import itertools
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
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from xgboost import XGBRegressor

from modeling import (
    METRICA_PRINCIPAL,
    METRICAS_MAYOR_ES_MEJOR,
    POLITICA_METRICAS,
    clip_no_negativo,
    metricas,
    metricas_target,
)

REFERENCIAS_METRICAS = [
    "Gneiting, T. (2011). Making and evaluating point forecasts. Journal of the American "
    "Statistical Association, 106(494), 746-762.",
    "Czado, C., Gneiting, T. y Held, L. (2009). Predictive model assessment for count data. "
    "Biometrics, 65(4), 1254-1261.",
    "Gneiting, T. y Resin, J. (2023). Regression diagnostics meets forecast evaluation: conditional "
    "calibration, reliability diagrams, and coefficient of determination. Electronic Journal of "
    "Statistics, 17(2), 3226-3286.",
    "Campbell, J. Y. y Thompson, S. B. (2008). Predicting excess stock returns out of sample: can "
    "anything beat the historical average? Review of Financial Studies, 21(4), 1509-1531.",
    "Kolassa, S. (2016). Evaluating predictive count data distributions in retail sales "
    "forecasting. International Journal of Forecasting, 32(3), 788-803.",
    "Walsh, C. y Joshi, A. (2024). Machine learning for sports betting: should model selection be "
    "based on accuracy or calibration? Machine Learning with Applications.",
    "Diebold, F. X. y Mariano, R. S. (1995). Comparing predictive accuracy. Journal of Business & "
    "Economic Statistics, 13(3), 253-263.",
    "White, H. (2000). A reality check for data snooping. Econometrica, 68(5), 1097-1126.",
    "Hansen, P. R. (2005). A test for superior predictive ability. Journal of Business & Economic "
    "Statistics, 23(4), 365-380.",
]
"""Fuentes de la politica de metricas; se guardan en la metadata de cada modelo."""

REQUISITOS = {"sesgo_adicional_maximo": 0.05, "pendiente_minima": 0.9, "pendiente_maxima": 1.1}
"""Umbrales para que un modelo sea elegible. Son convencion del proyecto, no vienen de la
literatura: la literatura pide que el modelo este calibrado y supere al promedio historico, y
estos valores fijan cuanto margen se acepta.

El sesgo se mide como sesgo adicional: |sesgo del modelo| menos |sesgo de predecir la media de
entrenamiento|, como fraccion del promedio real. Cualquier prediccion hecha con anios anteriores
hereda el cambio de nivel entre temporadas (en touchdowns, la tasa por receptor bajo de 0.216 en
2016-2021 a 0.193 en 2022-2023); el requisito revisa que el modelo no agregue mas de 5% encima de
ese cambio."""


def _es_mejor(nuevo, actual, criterio):
    if criterio in METRICAS_MAYOR_ES_MEJOR:
        return nuevo > actual
    return nuevo < actual


def _peor_valor(criterio):
    return -np.inf if criterio in METRICAS_MAYOR_ES_MEJOR else np.inf


def dividir_temporal(df, train_seasons, val_seasons, test_seasons, season_col="season"):
    """3 mascaras booleanas por temporada -- nunca aleatorio. Revienta si una
    temporada aparece en mas de una particion."""
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
    """Un fold por anio en `anios_test`: entrena con todo lo anterior a ese anio y prueba en
    ese anio (ventana expandida). Genera (anio, train_mask, test_mask)."""
    for anio in anios_test:
        yield anio, df[season_col] < anio, df[season_col] == anio


def _metricas_particiones(y, pred, train_mask, val_mask, test_mask, target):
    """Metricas en train/val/test, todas contra la media de entrenamiento. Si `target` no esta
    en la politica, solo las metricas basicas."""
    media = float(y[train_mask].mean())

    def medir(mascara):
        if target in POLITICA_METRICAS:
            return metricas_target(target, y[mascara], pred[mascara], media_entrenamiento=media)
        return metricas(y[mascara], pred[mascara], media_entrenamiento=media)

    return {
        "media_entrenamiento": media,
        "train": medir(train_mask),
        "val": medir(val_mask),
        "test": medir(test_mask),
    }


def evaluar_modelo(modelo, X, y, train_mask, val_mask, test_mask, ajustar=True, target=None):
    """Ajusta (si ajustar=True) sobre train y regresa prediccion + metricas en las 3
    particiones. Las predicciones se recortan en cero, igual que en el flujo semanal, para
    evaluar lo mismo que se va a usar. `target` define las metricas (por defecto, el nombre de
    `y`)."""
    if ajustar:
        modelo.fit(X[train_mask], y[train_mask])
    pred = pd.Series(clip_no_negativo(modelo.predict(X)), index=X.index)
    return {"modelo": modelo, "pred": pred,
            **_metricas_particiones(y, pred, train_mask, val_mask, test_mask, target or y.name)}


class BaselinePromedioJugador:
    """Predice el propio promedio de temporada del jugador (`{target}_season_avg`), con
    fallback a `{target}_career_avg` y despues a la media de entrenamiento: la referencia
    minima que cualquier modelo tiene que superar."""

    def __init__(self, target):
        self.target = target

    def fit(self, X, y):
        self.media_train_ = y.mean()
        return self

    def predict(self, X):
        season = X[f"{self.target}_season_avg"]
        career = X[f"{self.target}_career_avg"]
        return season.fillna(career).fillna(self.media_train_).to_numpy()


def resumen_resultados(resultados, criterio):
    """De un dict {nombre: salida de evaluar_modelo()} arma un DataFrame con todas las metricas
    por particion, ordenado por `criterio` en validacion (se elige en validacion y la prueba
    solo se reporta)."""
    filas = {
        nombre: {
            f"{parte}_{metrica}": valor
            for parte in ("train", "val", "test")
            for metrica, valor in salida[parte].items()
        }
        for nombre, salida in resultados.items()
    }
    return pd.DataFrame(filas).T.sort_values(
        f"val_{criterio}", ascending=criterio not in METRICAS_MAYOR_ES_MEJOR
    )


def entrenar_y_evaluar(df, target, features_arbol, features_ridge, train_mask, val_mask, test_mask,
                       semilla=0, criterio=None):
    """Compara Baseline / Ridge / RandomForest / XGBoost / HistGradientBoosting para `target`.
    Ridge usa `features_ridge` (imputado + escalado); los arboles usan `features_arbol` sin
    escalar (XGBoost/HistGradientBoosting manejan NaN nativo, RandomForest se imputa aparte
    porque sklearn no acepta NaN ahi). Dentro de cada familia, los hiperparametros se eligen con
    una busqueda pequena por `criterio` en validacion (por defecto, la metrica principal del
    target).

    Regresa (resultados, resumen): `resultados` es un dict {nombre: salida de
    evaluar_modelo()}; `resumen` es la tabla de resumen_resultados()."""
    criterio = criterio or METRICA_PRINCIPAL[target]
    y = df[target]
    resultados = {}

    def quedarse_con_mejor(nombre, salida):
        actual = resultados.get(nombre)
        if actual is None or _es_mejor(salida["val"][criterio], actual["val"][criterio], criterio):
            resultados[nombre] = salida

    base = BaselinePromedioJugador(target).fit(df[train_mask], y[train_mask])
    resultados["Baseline"] = evaluar_modelo(base, df, y, train_mask, val_mask, test_mask, ajustar=False)

    X_ridge = pd.get_dummies(df[features_ridge], dummy_na=True)
    for alpha in (0.1, 1.0, 10.0, 50.0):
        pipe = Pipeline([
            ("imputar", SimpleImputer(strategy="median")),
            ("escalar", StandardScaler()),
            ("modelo", Ridge(alpha=alpha)),
        ])
        quedarse_con_mejor("Ridge", evaluar_modelo(pipe, X_ridge, y, train_mask, val_mask, test_mask))

    X_arbol = df[features_arbol]
    X_arbol_imputado = pd.DataFrame(
        SimpleImputer(strategy="median").fit(X_arbol[train_mask]).transform(X_arbol),
        index=X_arbol.index,
        columns=X_arbol.columns,
    )
    for max_depth in (6, 10, 14):
        for min_leaf in (3, 5, 10):
            modelo = RandomForestRegressor(
                n_estimators=300, max_depth=max_depth, min_samples_leaf=min_leaf,
                random_state=semilla, n_jobs=-1,
            )
            quedarse_con_mejor("RandomForest", evaluar_modelo(modelo, X_arbol_imputado, y, train_mask, val_mask, test_mask))

    for max_depth in (3, 4, 6):
        for n_estimators in (200, 400):
            for lr in (0.03, 0.08):
                modelo = XGBRegressor(
                    max_depth=max_depth, n_estimators=n_estimators, learning_rate=lr,
                    random_state=semilla, n_jobs=-1,
                )
                quedarse_con_mejor("XGBoost", evaluar_modelo(modelo, X_arbol, y, train_mask, val_mask, test_mask))

    for max_iter in (100, 200, 300):
        for max_depth in (None, 6, 10):
            modelo = HistGradientBoostingRegressor(max_iter=max_iter, max_depth=max_depth, random_state=semilla)
            quedarse_con_mejor("HistGradientBoosting", evaluar_modelo(modelo, X_arbol, y, train_mask, val_mask, test_mask))

    return resultados, resumen_resultados(resultados, criterio)


ESPACIO_COMPACTO = {
    "rmse": {"max_depth": [3, 4, 5], "learning_rate": [0.03, 0.05]},
    "deviance_poisson": {"tweedie_variance_power": [1.2, 1.35, 1.5, 1.65, 1.8], "max_depth": [2, 3, 4]},
}
"""Busqueda por defecto, deliberadamente pequena: cada combinacion encuentra su numero de
arboles con early stopping, asi que no hace falta buscarlo. Para touchdowns se busca la potencia
de Tweedie y la profundidad, con learning_rate fija en 0.03."""

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
"""Espacio amplio (regularizacion y submuestreo), solo como opcion: una busqueda aleatoria de 100
intentos sobre el no encontro mejoras relevantes y es mucho mas costosa."""

EVAL_METRIC_XGBOOST = {"rmse": "rmse", "deviance_poisson": "poisson-nloglik"}
"""Metrica de XGBoost para early stopping, alineada con el criterio: la log-verosimilitud de
Poisson es, para un mismo resultado, la deviance de Poisson salvo una constante."""


def buscar_hiperparametros(df, target, features, train_mask, val_mask, espacio=None, params_fijos=None,
                           n_intentos=None, semilla=0, criterio=None):
    """Busqueda de hiperparametros de XGBoost, elegida por `criterio` en validacion (por
    defecto, la metrica principal del target). `espacio` es un dict de listas: sin `n_intentos`
    se prueban todas las combinaciones (por defecto ESPACIO_COMPACTO del criterio); con
    `n_intentos` se muestrean al azar. `n_estimators` no se busca: se pone un maximo (2000) y
    early stopping sobre validacion (50 rondas sin mejorar), con la metrica de XGBoost alineada
    al criterio.

    `params_fijos`: hiperparametros que no se buscan (ej. `{"objective": "reg:tweedie"}`).

    Regresa (mejor_modelo, mejor_config, tabla_intentos). Solo se conserva el mejor modelo;
    `tabla_intentos` trae, por combinacion, los hiperparametros (fijos y buscados) y sus
    metricas de validacion."""
    criterio = criterio or METRICA_PRINCIPAL[target]
    espacio = espacio or ESPACIO_COMPACTO[criterio]
    if criterio == "deviance_poisson":
        params_fijos = {"objective": "reg:tweedie", "learning_rate": 0.03, **(params_fijos or {})}
    else:
        params_fijos = params_fijos or {}
    X, y = df[features], df[target]
    media = float(y[train_mask].mean())

    claves = list(espacio)
    combinaciones = [dict(zip(claves, valores)) for valores in itertools.product(*(espacio[k] for k in claves))]
    if n_intentos is not None:
        rng = np.random.RandomState(semilla)
        elegidas = rng.choice(len(combinaciones), size=min(n_intentos, len(combinaciones)), replace=False)
        combinaciones = [combinaciones[i] for i in elegidas]

    intentos = []
    mejor_valor, mejor_modelo, mejor_config = _peor_valor(criterio), None, None
    for config in combinaciones:
        params = {**params_fijos, **config, "n_estimators": 2000, "random_state": semilla, "n_jobs": -1}
        modelo = XGBRegressor(**params, early_stopping_rounds=50, eval_metric=EVAL_METRIC_XGBOOST[criterio])
        modelo.fit(X[train_mask], y[train_mask], eval_set=[(X[val_mask], y[val_mask])], verbose=False)

        medidas = metricas_target(target, y[val_mask], clip_no_negativo(modelo.predict(X[val_mask])), media)
        intentos.append({**params_fijos, **config, "n_estimators_optimo": modelo.best_iteration + 1,
                         **{f"val_{k}": v for k, v in medidas.items()}})
        if _es_mejor(medidas[criterio], mejor_valor, criterio):
            mejor_valor, mejor_modelo = medidas[criterio], modelo
            mejor_config = {**{k: v for k, v in params.items() if k not in ("n_jobs",)},
                            "n_estimators": modelo.best_iteration + 1}

    tabla_intentos = pd.DataFrame(intentos).sort_values(
        f"val_{criterio}", ascending=criterio not in METRICAS_MAYOR_ES_MEJOR
    ).reset_index(drop=True)
    return mejor_modelo, mejor_config, tabla_intentos


def entrenar_hurdle(df, target, features_arbol, train_mask, val_mask, test_mask, semilla=0):
    """Modelo de 2 etapas para un target dominado por ceros (ej. receiving_tds): etapa 1
    clasifica P(target > 0 | features) con HistGradientBoostingClassifier; etapa 2 regresa el
    conteo esperado condicional a ser positivo (HistGradientBoostingRegressor(loss="poisson"),
    entrenado SOLO sobre filas con target > 0). Prediccion final = P(etapa 1) * E[etapa 2].
    Regresa el mismo formato que evaluar_modelo()."""
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
    return {"modelo": (clasificador, regresor), "pred": pred,
            **_metricas_particiones(y, pred, train_mask, val_mask, test_mask, target)}


def spearman_semanal(df, y, pred, minimo_filas=20):
    """Correlacion de Spearman entre lo real y lo predicho dentro de cada semana, promediada entre
    semanas: mide si el modelo ordena bien a los receptores de una misma semana, que es la
    decision de fantasy (a quien alinear). `df`, `y` y `pred` comparten indice; se omiten las
    semanas con menos de `minimo_filas` receptores."""
    tabla = pd.DataFrame({"season": df["season"], "week": df["week"], "y": y, "pred": pred})
    correlaciones = [
        grupo["y"].corr(grupo["pred"], method="spearman")
        for _, grupo in tabla.groupby(["season", "week"])
        if len(grupo) >= minimo_filas and grupo["pred"].nunique() > 1
    ]
    return float(np.nanmean(correlaciones))


def _perdida_por_fila(y, pred, criterio):
    y = np.asarray(y, dtype=float)
    pred = np.asarray(pred, dtype=float)
    if criterio == "rmse":
        return (y - pred) ** 2
    if criterio == "deviance_poisson":
        tasa = np.clip(pred, 1e-6, None)
        termino = np.where(y > 0, y * np.log(np.where(y > 0, y, 1) / tasa), 0.0)
        return 2 * (termino - (y - tasa))
    raise ValueError(f"criterio sin perdida por fila: {criterio}")


def diferencia_bootstrap(df, y, pred_a, pred_b, criterio, n_remuestreos=2000, semilla=0, nivel=0.95):
    """Diferencia en `criterio` (modelo a menos modelo b) con intervalo de `nivel` por bootstrap de
    semanas completas: se remuestrean semanas (season, week) y no filas, porque los errores de una
    misma semana estan correlacionados. Es una version por remuestreo de la pregunta de Diebold y
    Mariano (1995): si dos pronosticos tienen igual capacidad predictiva. Si el intervalo incluye
    cero, la diferencia no se distingue del ruido. Con varias alternativas contra el mismo modelo,
    `nivel` permite ajustar por comparaciones multiples (Bonferroni: 1 - 0.05 / comparaciones).
    Regresa {diferencia, ic_inferior, ic_superior, empate}."""
    tabla = pd.DataFrame({
        "semana": df["season"].astype(str) + "-" + df["week"].astype(str),
        "a": _perdida_por_fila(y, pred_a, criterio),
        "b": _perdida_por_fila(y, pred_b, criterio),
    })
    por_semana = tabla.groupby("semana").agg(a=("a", "sum"), b=("b", "sum"), n=("a", "size"))
    a, b, n = por_semana["a"].to_numpy(), por_semana["b"].to_numpy(), por_semana["n"].to_numpy()

    def metrica(suma, filas):
        return np.sqrt(suma / filas) if criterio == "rmse" else suma / filas

    rng = np.random.RandomState(semilla)
    indices = rng.randint(0, len(n), size=(n_remuestreos, len(n)))
    diferencias = metrica(a[indices].sum(axis=1), n[indices].sum(axis=1)) - metrica(b[indices].sum(axis=1), n[indices].sum(axis=1))
    alfa = 1 - nivel
    ic_inferior, ic_superior = np.percentile(diferencias, [100 * alfa / 2, 100 * (1 - alfa / 2)])
    return {
        "diferencia": float(metrica(a.sum(), n.sum()) - metrica(b.sum(), n.sum())),
        "ic_inferior": float(ic_inferior),
        "ic_superior": float(ic_superior),
        "empate": bool(ic_inferior <= 0 <= ic_superior),
    }


def cumple_requisitos(target, metricas_validacion, metricas_por_anio=None):
    """Revisa si un modelo es elegible: R2 fuera de muestra > 0 (y D2 > 0 si la metrica
    principal es la deviance) en validacion y en cada anio del walk-forward, sesgo relativo y
    pendiente de calibracion dentro de REQUISITOS. `metricas_por_anio`: {anio: metricas}.
    Regresa (cumple, detalle) con un renglon por chequeo."""
    usa_d2 = METRICA_PRINCIPAL[target] == "deviance_poisson"
    detalle = {"R2 fuera de muestra > 0 (validacion)": metricas_validacion["r2_oos"] > 0}
    if usa_d2:
        detalle["D2 fuera de muestra > 0 (validacion)"] = metricas_validacion["d2_oos"] > 0
    sesgo_adicional = (
        abs(metricas_validacion["sesgo"]) - abs(metricas_validacion.get("sesgo_media_entrenamiento", 0.0))
    ) / metricas_validacion["media_real"]
    detalle[f"sesgo adicional al de la media de entrenamiento <= {REQUISITOS['sesgo_adicional_maximo']:.0%} de la media (validacion)"] = (
        sesgo_adicional <= REQUISITOS["sesgo_adicional_maximo"]
    )
    detalle[f"pendiente de calibracion entre {REQUISITOS['pendiente_minima']} y {REQUISITOS['pendiente_maxima']} (validacion)"] = (
        REQUISITOS["pendiente_minima"] <= metricas_validacion["pendiente_calibracion"] <= REQUISITOS["pendiente_maxima"]
    )
    if metricas_por_anio:
        detalle["R2 fuera de muestra > 0 en cada anio"] = all(m["r2_oos"] > 0 for m in metricas_por_anio.values())
        if usa_d2:
            detalle["D2 fuera de muestra > 0 en cada anio"] = all(m["d2_oos"] > 0 for m in metricas_por_anio.values())
    return all(detalle.values()), detalle


def guardar_modelo(modelo, carpeta, target, variables, media_entrenamiento, metricas_referencia,
                   entrenado_con, limitaciones=()):
    """Guarda un XGBRegressor ya evaluado: el archivo del modelo ({target}_xgboost.json, formato
    nativo de XGBoost) y su metadata ({target}_metadata.json): variables en el orden exacto del
    entrenamiento, hiperparametros, media del target en entrenamiento (referencia de R2/D2 fuera
    de muestra cada semana), metrica principal, metricas de referencia (dict con
    "validacion", "prueba" y "walk_forward"), de que datos salio y las fuentes de la politica de
    metricas. Es lo unico que modeling.cargar_modelos() necesita para aplicarlo cada semana."""
    carpeta = Path(carpeta)
    carpeta.mkdir(parents=True, exist_ok=True)
    modelo.save_model(str(carpeta / f"{target}_xgboost.json"))
    metadata = {
        "target": target,
        "modelo": type(modelo).__name__,
        "hiperparametros": {k: v for k, v in modelo.get_params().items() if v is not None},
        "features_arbol_en_orden": list(variables),
        "media_entrenamiento": float(media_entrenamiento),
        "metrica_principal": METRICA_PRINCIPAL[target],
        "entrenado_con": entrenado_con,
        "fecha_de_guardado": pd.Timestamp.now(tz="UTC").isoformat(),
        "metricas_referencia": metricas_referencia,
        "requisitos": REQUISITOS,
        "referencias": REFERENCIAS_METRICAS,
        "limitaciones_conocidas": list(limitaciones),
    }
    (carpeta / f"{target}_metadata.json").write_text(json.dumps(metadata, indent=2, ensure_ascii=False, default=float))
