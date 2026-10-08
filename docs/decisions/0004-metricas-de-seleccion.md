# 0004 — Métricas de selección de modelos

**Fecha:** 2026-10-07
**Estado:** aceptada

## Contexto

Los modelos de WR predicen **valores esperados**: cuántas recepciones, yardas y touchdowns tendrá en
promedio un receptor con ese perfil. Esos valores se suman para armar puntos de fantasy. La selección
de modelos usaba MAE, una métrica heredada de los notebooks originales.

Evidencia (validación 2022-2023, `4.1_preparacion_y_metricas.ipynb` y
`4.3_modelos_touchdowns.ipynb`):

- MAE premia la mediana, no la media. En recepciones y yardas, predecir la mediana de entrenamiento
  gana en MAE y predecir la media gana en RMSE.
- En touchdowns (82% de ceros) la mediana es 0. Predecir 0 para todos tiene MAE de 0.193 contra
  0.336 de la media de entrenamiento, pero D² de −6.13.
- El modelo de touchdowns que se había elegido por MAE (XGBoost Tweedie con hiperparámetros por
  defecto) quedó descalibrado: pendiente de calibración 0.57, D² de −0.206 en validación y R² de
  −0.040 en prueba. Su descalibración (0.0134) es mayor que su discriminación (0.0112).
- Tres decisiones se habían tomado mirando el conjunto de prueba (ver `docs/HALLAZGOS.md`).

## Opciones consideradas

1. **Mantener MAE** — fácil de leer, pero premia la mediana y en touchdowns favorece predecir cero.
2. **RMSE para los tres resultados** — consistente con la media, pero en un conteo raro pesa poco la
   diferencia entre predecir 0.02 y 0.2 cuando el evento ocurre.
3. **Política por resultado, con requisitos de calibración** — la métrica que decide es consistente
   con la media en cada caso y es la propia del tipo de dato; se exige además superar al promedio
   histórico y estar calibrado.

## Decisión

Opción 3.

| Resultado | Métrica que decide (en validación) | También se reporta |
|---|---|---|
| `receptions` | RMSE | MAE, deviance de Poisson, R² fuera de muestra, sesgo, pendiente de calibración, Spearman semanal |
| `receiving_yards` | RMSE | MAE, R² fuera de muestra, sesgo, pendiente de calibración, Spearman semanal |
| `receiving_tds` | Deviance de Poisson | D² y R² fuera de muestra, Brier y AUC de «anota al menos uno», sesgo, pendiente, RMSE; MAE solo informativo |

Reglas:

- **Se elige en validación (2022-2023); la prueba (2024-2025) solo se reporta.** La verificación sin
  ningún uso previo es la temporada en curso.
- **R² y D² se miden fuera de muestra**, contra la media de entrenamiento (Campbell y Thompson,
  2008). Un valor negativo significa peor que el promedio histórico.
- **Requisitos para ser elegible** (`experimentos.cumple_requisitos`): R² fuera de muestra mayor a 0
  en validación y en cada año de la evaluación año por año (y D² mayor a 0 en touchdowns); pendiente
  de calibración entre 0.9 y 1.1; sesgo adicional al de la media de entrenamiento de a lo más 5% del
  promedio real. El sesgo se mide como adicional porque cualquier predicción hecha con años anteriores
  hereda el cambio de nivel entre temporadas: la tasa de touchdowns por receptor bajó de 0.216
  (2016-2021) a 0.193 (2022-2023). Los umbrales son convención del proyecto.
- **Empates:** si la diferencia entre dos modelos en la métrica que decide tiene un intervalo de 95%
  que incluye cero (bootstrap de semanas completas, en el espíritu de Diebold y Mariano, 1995), se
  considera empate y se elige XGBoost, la familia que aplica el flujo semanal.
- **Varias alternativas contra el mismo modelo:** un intervalo de 95% deja 5% de probabilidad de que
  una alternativa que no es mejor lo parezca por azar; con k alternativas para la misma pregunta, la
  probabilidad de que al menos una lo parezca crece con k (*data snooping*: White, 2000; Hansen,
  2005). En ese caso cada intervalo se calcula con nivel 1 − 0.05/k (corrección de Bonferroni,
  `experimentos.diferencia_bootstrap(..., nivel=...)`). k cuenta todas las alternativas evaluadas para
  esa pregunta, de los tres resultados; en una búsqueda de hiperparámetros, las configuraciones
  probadas. Un intervalo más ancho solo puede convertir una mejora en empate: lo que empata al 95%
  sigue empatando.
- **Ajustes** (hiperparámetros, variables propias por resultado, ventanas de entrenamiento, mezclas)
  se adoptan solo si mejoran la métrica que decide en validación con una diferencia cuyo intervalo,
  ajustado si se probaron varias alternativas, no incluye cero.

La regla de comparaciones múltiples se agregó al revisar `4.9_ventana_entrenamiento.ipynb`: de 15
alternativas, la ponderación por antigüedad en recepciones excluía el cero con el intervalo de 95% y
no con el ajustado. Después se corrigió un error en los datos de alineaciones (ver
`docs/HALLAZGOS.md`) y, con el dato corregido, esa alternativa ya empata con el intervalo de 95%. En
la etapa 4 ninguna alternativa supera al modelo elegido ni con el intervalo de 95%, así que la regla
no cambia ninguna elección de modelo. Sí cambia una conclusión de la evaluación: en
`5.2_benchmark_vs_legado.ipynb`, la mejora en recepciones contra el flujo heredado excluye el cero
con 95% pero no con el intervalo ajustado por las tres comparaciones.

## Consecuencias

- `modeling.py`: `POLITICA_METRICAS`, `METRICA_PRINCIPAL`, `metricas()` y `metricas_target()`. El
  archivo semanal de métricas reporta la métrica principal de cada modelo y su referencia de
  validación y prueba.
- `experimentos.py`: el criterio por defecto es la métrica principal de cada resultado en
  `entrenar_y_evaluar`, `resumen_resultados` y `buscar_hiperparametros` (con early stopping en la
  métrica correcta); se agregan `diferencia_bootstrap` (con `nivel` para el ajuste por comparaciones
  múltiples), `cumple_requisitos`, `spearman_semanal` y `REFERENCIAS_METRICAS`. `guardar_modelo` guarda la media de entrenamiento, la métrica principal,
  los requisitos y las referencias en la metadata de cada modelo.
- Los notebooks se reorganizaron por etapa (`weekly/wr/notebooks/README.md`); la etapa 4 aplica esta
  política y la etapa 6 entrena y guarda los modelos que usa el flujo semanal.
- Fuera de alcance: la selección de variables (`3.2_seleccion_features.ipynb`) sigue midiendo
  importancia con MAE y usó como validación los años que después son prueba (ver
  `docs/HALLAZGOS.md`).

## Referencias

- Gneiting, T. (2011). [Making and evaluating point forecasts](https://arxiv.org/abs/0912.0902).
  *Journal of the American Statistical Association*, 106(494), 746-762.
- Czado, C., Gneiting, T. y Held, L. (2009).
  [Predictive model assessment for count data](https://www.zora.uzh.ch/entities/publication/3a6b88b2-4bc3-4929-80ef-010bdd2b69ce).
  *Biometrics*, 65(4), 1254-1261.
- Gneiting, T. y Resin, J. (2023).
  [Regression diagnostics meets forecast evaluation](https://projecteuclid.org/journals/electronic-journal-of-statistics/volume-17/issue-2/Regression-diagnostics-meets-forecast-evaluation-conditional-calibration-reliability-diagrams/10.1214/23-EJS2180.full).
  *Electronic Journal of Statistics*, 17(2), 3226-3286.
- Campbell, J. Y. y Thompson, S. B. (2008).
  [Predicting excess stock returns out of sample](https://nber.org/papers/w11468). *Review of
  Financial Studies*, 21(4), 1509-1531.
- Kolassa, S. (2016). Evaluating predictive count data distributions in retail sales forecasting.
  *International Journal of Forecasting*, 32(3), 788-803.
- Walsh, C. y Joshi, A. (2024).
  [Machine learning for sports betting: should model selection be based on accuracy or calibration?](https://researchportal.bath.ac.uk/en/publications/machine-learning-for-sports-betting-should-model-selection-be-bas/)
  *Machine Learning with Applications*.
- Diebold, F. X. y Mariano, R. S. (1995). Comparing predictive accuracy. *Journal of Business &
  Economic Statistics*, 13(3), 253-263.
- White, H. (2000). A reality check for data snooping. *Econometrica*, 68(5), 1097-1126.
- Hansen, P. R. (2005). A test for superior predictive ability. *Journal of Business & Economic
  Statistics*, 23(4), 365-380.
- FantasyPros, [metodología de exactitud semanal](https://fantasypros.com/about/faq/football-inseason-accuracy-methodology).
