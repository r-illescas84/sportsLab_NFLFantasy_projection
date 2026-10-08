# 0006 — Seguimiento semanal y ciclo de vida del modelo

**Fecha:** 2026-10-07
**Estado:** aceptada

## Contexto

El flujo semanal (`weekly/wr/src/pipeline.py`) predice la semana pendiente y, cuando la semana ya se
jugó, escribe sus métricas contra lo real. Le faltaban tres cosas para operarse durante la temporada:

- **Historial.** Cada semana quedaba en un archivo suelto y nada avisaba si el modelo se degradaba.
  Ya había señales que vigilar: en las semanas 1-4 de 2026 el modelo subestima 3.1 yardas por
  receptor y la pendiente de calibración de yardas queda en 1.105, fuera del rango 0.9-1.1
  (`6.1_modelo_final_y_temporada_actual.ipynb`).
- **Auditoría.** Al evaluar una semana jugada, el flujo volvía a predecir con los modelos de ese
  momento y sobrescribía `predicciones_semana_N.csv`. Lo que se había predicho antes de los partidos
  se perdía, y el DATASHEET marcaba la vigencia como «no auditable».
- **Calidad recurrente.** Las seis dimensiones de calidad de `docs/data/DATASHEET.md` se auditaron
  una vez, al construir los datos, y no se revisaban en cada corrida.

## Decisión

### Qué predicción se evalúa

Opciones: la emitida antes de los partidos o una recalculada después. **Se evalúa la emitida**,
porque es la que estuvo disponible para decidir. Si no existe (semanas anteriores a este flujo), se
recalcula con los modelos vigentes y queda marcada como `reconstruida`. Volver a correr una semana
pendiente antes del primer partido reemplaza la emitida: cuenta la última anterior a los partidos.
Una vez que empieza la semana, el flujo ya no la acepta como pendiente, así que la emitida no puede
cambiar.

### Qué se guarda de los datos de entrada

Opciones: una huella de las fuentes, la tabla de variables de la semana con su huella, o una copia
de las fuentes completas. **Se guarda la tabla de variables** (`variables_semana_N.csv`, unos 60 KB
por semana) y su huella (los primeros 16 caracteres de su SHA-256) queda en el historial. La
predicción se hace leyendo ese mismo archivo, así que con él y el modelo guardado se reproduce exacta.
Una copia de las fuentes pesaría varios MB por semana sin agregar nada para reproducir la predicción.

### Chequeos de calidad en cada corrida

Opciones: que todo falle bloquee, que todo solo advierta, o que **bloquee solo lo que invalida la
predicción** (elegida). Lo demás queda como advertencia en el historial
(`seguimiento.chequear_calidad`):

| Dimensión | Chequeo | Nivel |
|---|---|---|
| Unicidad | Una fila por jugador en la semana | Bloquea |
| Completitud | Las variables de los modelos existen y ninguna está 100% vacía (si en el histórico de esa semana no lo estaba) | Bloquea |
| Completitud | Vacíos a no más de 10 puntos sobre el histórico de la misma semana | Advierte |
| Consistencia | Cada equipo que juega tiene receptores (alineación completa) | Bloquea en semana pendiente |
| Consistencia | Los equipos de la tabla están en el calendario de la semana | Advierte |
| Validez | Valores dentro del rango histórico de cada variable | Advierte |
| Exactitud | En semana jugada: `fantasy_points_ppr` = `fantasy_points` + recepciones; recepciones ≤ pases dirigidos | Advierte |
| Vigencia | Semana sin partidos a medias (ya existía); estadísticas de la semana anterior cargadas | Bloquea |
| Vigencia | Líneas de apuestas en cada partido; predicción emitida encontrada y hecha con los modelos vigentes | Advierte |

### Límites de control

Cada semana jugada se miden juntas las cuatro últimas semanas de la temporada (una ventana): la
métrica principal, el sesgo y la pendiente de calibración. Los límites salen de repetir esa
operación en 2021-2025 con el walk-forward de `5.1_estabilidad_temporal.ipynb` (el modelo de cada año
entrenado con los anteriores): 75 ventanas por resultado, de la 1-4 a la 15-18 de cada temporada
(`experimentos.ventanas_walk_forward` y `experimentos.limites_de_seguimiento`). Se calculan en
`6.1_modelo_final_y_temporada_actual.ipynb` y se guardan en la metadata de cada modelo.

- **Percentiles con Bonferroni.** Se vigilan nueve series a la vez (tres resultados por tres
  métricas), así que el 5% se reparte entre ellas, como en el ADR 0004: percentiles 0.28 y 99.72.
  La métrica principal alerta solo hacia arriba; el sesgo y la pendiente, de los dos lados.
- **Alerta persistente.** La misma métrica fuera también en la ventana que termina cuatro semanas
  antes, la primera sin semanas en común. Dos ventanas seguidas comparten tres de sus cuatro semanas
  y casi no agregan evidencia.

La primera regla que se consideró usaba percentiles 2.5 y 97.5 por serie, y contaba como persistente
estar fuera en dos ventanas seguidas. Se probó dejando fuera una temporada a la vez: los límites se
calculan con las otras cuatro y se cuentan las alertas en la que quedó fuera
(`6.3_seguimiento_semanal.ipynb`). En las cinco temporadas, en las que el modelo cumplió los
requisitos, habría habido alguna alerta persistente. Cada corrección por separado deja 3 y 4
temporadas de 5; juntas, una (la pendiente de recepciones en la semana 18 de 2024). Las alertas
sueltas siguen apareciendo todas las temporadas: 5.5% de las combinaciones de ventana y métrica
quedan fuera de los límites.

### Cuándo se reentrena

Opciones: cada semana, una vez por temporada o cuando haya alerta.

- **Al cerrar la temporada regular, siempre**: se reentrena con la temporada completa
  re-ejecutando `6.1_modelo_final_y_temporada_actual.ipynb`, que también recalcula los límites.
- **A mitad de temporada, solo con alerta persistente y una revisión en notebook que lo confirme.**
  La revisión separa un cambio de nivel de toda la liga (el sesgo que tendría predecir la media de
  entrenamiento, en `metricas_semana_N.csv`) de algo propio del modelo.
- **El flujo semanal nunca reentrena solo.** Un modelo nuevo siempre pasa por 6.1, que lo evalúa y lo
  guarda con su metadata.

No se reentrena cada semana: mezclaría modelos dentro de una temporada, y en
`4.9_ventana_entrenamiento.ipynb` darle más peso a lo reciente o acortar la ventana de entrenamiento
no mejoró (empates o peor).

### Semanas 1-4 de 2026

Se cargan al historial como `reconstruida`: las predicciones emitidas en su momento eran de modelos
anteriores a la selección de variables del ADR 0005. La primera predicción que se evalúa tal como se
emitió es la de la semana 5.

## Consecuencias

- `weekly/wr/src/seguimiento.py` (nuevo, corre cada semana): chequeos, huella, ventana, alertas e
  historial. `pipeline.py` guarda la predicción emitida y sus variables, evalúa sin modificarlas
  (`evaluacion_semana_N.csv`) y agrega sus filas a `weekly/wr/outputs/tracking.csv`, una por corrida
  y resultado; las filas nunca se reescriben. `modeling.cargar_modelos` devuelve los límites y la
  fecha de guardado de cada modelo.
- La ventana 1-4 de 2026 ya da una alerta suelta: el sesgo de yardas (−3.07 contra un límite de
  −1.84). Sería persistente si la ventana 5-8 también quedara fuera.
- La vigencia del DATASHEET pasa a ser auditable desde la semana 5 de 2026.
- La corrida sigue siendo manual (`python weekly/wr/src/pipeline.py --season 2026 --week N`), antes
  de los partidos y después de que terminen.

## Referencias

- Montgomery, D. C. (2019). *Introduction to Statistical Quality Control* (8.ª ed.). Wiley. Límites
  de control a partir de un periodo de referencia, y por qué las reglas adicionales de alarma suben
  la tasa de falsas alarmas.
- Sculley, D., Holt, G., Golovin, D., Davydov, E., Phillips, T., Ebner, D., Chaudhary, V., Young, M.,
  Crespo, J.-F. y Dennison, D. (2015). Hidden technical debt in machine learning systems. *Advances
  in Neural Information Processing Systems*, 28.
- Breck, E., Cai, S., Nielsen, E., Salib, M. y Sculley, D. (2017). The ML test score: a rubric for ML
  production readiness and technical debt reduction. *IEEE International Conference on Big Data*.
