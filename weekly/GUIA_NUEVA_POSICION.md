# Guía para una posición nueva

Cómo adaptar el flujo semanal de WR (`weekly/wr/`) a otra posición. WR se trabajó primero y
completo, para servir de referencia a QB, RB y TE
([ADR 0003](../docs/decisions/0003-alcance-wr-semanal.md)): datos, análisis exploratorio,
variables, modelado, evaluación, modelo final y seguimiento semanal. Esta guía dice qué se copia
tal cual, qué se cambia y qué se vuelve a decidir con los datos de la posición.

## 1. La forma que se repite

```
weekly/<posicion>/
  src/        cada semana: data.py -> features.py -> seguimiento.py -> modeling.py -> pipeline.py
              solo notebooks: experimentos.py, features_exploratorio.py
  notebooks/  etapas 1 a 6, numeradas etapa.paso
  models/     los modelos que aplica el flujo, con su metadata; solo los escribe el notebook 6.1
  outputs/    predicción emitida, variables, evaluación y métricas por semana; tracking.csv
```

Lo que corre cada semana vive en `src/` y nunca entrena: decidir y evaluar modelos se hace en los
notebooks ([`docs/PLAN.md`](../docs/PLAN.md), regla de escalabilidad). La página
[`docs/arquitectura/pipeline_wr_semanal.html`](../docs/arquitectura/pipeline_wr_semanal.html)
muestra qué recibe, contiene y entrega cada módulo.

## 2. Primeros pasos

1. Copiar `weekly/wr/` a `weekly/<posicion>/`, sin `models/`, `outputs/` ni
   `notebooks/_reference/` (los notebooks heredados de WR).
2. Cambiar `POSICION` en `src/features.py`. En corredores, decidir antes qué códigos de alineación
   cuentan (sección 6).
3. Cambiar `TARGETS` y `POLITICA_METRICAS` en `src/modeling.py`, y `COLUMNA_ORDEN` en
   `src/pipeline.py`.
4. Rehacer las etapas en orden (sección 3). `COLUMNAS_BASE_MODELADO` y
   `FEATURES_SELECCIONADAS_ARBOL` de `src/features.py` salen de la etapa 3.
5. Ejecutar 6.1, que guarda los modelos y sus límites de seguimiento, y correr la semana como
   indica [`weekly/README.md`](README.md).

## 3. Las etapas, con su ejemplo en WR

| Etapa | Pregunta | En WR | Qué cambia en la posición nueva |
|---|---|---|---|
| 1. Datos | ¿Los datos son confiables y llegan a tiempo? | 1.1 a 1.4 | Repetir la auditoría con las filas de la posición: nulos, duplicados, cobertura de la alineación. `data.py` no cambia. |
| 2. Análisis exploratorio | ¿Cómo se comporta cada resultado y qué lo explica? | 2.1 a 2.6 | Todo: los resultados y lo que los explica son de la posición. Cada resultado se explica con un caso real antes de los números. |
| 3. Variables | ¿Qué variables necesitan los modelos? | 3.1 a 3.3 | Las candidatas. La selección se repite con las mismas funciones y la regla del ADR 0005. |
| 4. Modelado | ¿Qué modelo predice mejor cada resultado? | 4.1 a 4.9 | La métrica principal de cada resultado y la comparación de familias. Las alternativas de 4.5 a 4.9 no se adoptaron en WR; repetirlas es opcional. |
| 5. Evaluación | ¿Es estable, es mejor que lo anterior y se puede explicar? | 5.1 a 5.3 | 5.2 solo aplica si la posición tiene un flujo anterior con el que comparar. |
| 6. Modelo final y seguimiento | ¿Qué modelo corre cada semana y cómo se vigila? | 6.1 a 6.3 | Se vuelven a ejecutar: entrenan con toda la historia, calculan los límites y guardan. |

El índice de notebooks de WR, con la pregunta de cada uno, está en
[`wr/notebooks/README.md`](wr/notebooks/README.md).

## 4. Qué se copia y qué se cambia en el código

| Archivo | Se usa tal cual | Se cambia |
|---|---|---|
| `data.py` | Todo: temporada regular, códigos de equipo actuales, alineaciones de los dos esquemas unidas (`cargar_depth_charts_unificado`, con `posicion`) e `identificar_qb_titular` | Nada |
| `features.py` | Promedios sin fuga (`agregar_promedios_jugador`), edad y experiencia, draft, total implícito, `construir_tabla_modelado`, `filas_de_la_semana` | `POSICION`; `COLUMNAS_BASE_MODELADO` (las estadísticas de la posición); `FEATURES_SELECCIONADAS_ARBOL` (de 3.3); qué producción del equipo suma `agregar_ofensiva_equipo`; `calcular_ratios_eficiencia`, que es de recepción (sirve para TE); la tabla de decisiones del encabezado |
| `features_exploratorio.py` | Volatilidad, cambio de QB titular o de equipo, `acotar_variable` | Las candidatas (`COLUMNAS_BASE_CANDIDATAS`, `CANDIDATAS_CONTEXTO`) y `construir_tabla_candidatas`; `VARIABLES_SELECCION_3_2` es historia de WR y se quita |
| `modeling.py` | `metricas`, `cargar_modelos`, `predecir`, `evaluar_predicciones` | `TARGETS` y `POLITICA_METRICAS` |
| `experimentos.py` | Todo | Nada |
| `seguimiento.py` | Todo; los chequeos de exactitud valen para cualquier posición | Nada |
| `pipeline.py` | Todo | `COLUMNA_ORDEN` |

## 5. Lo que se vuelve a decidir y lo que no

Se decide otra vez, con los datos de la posición:

- **Los resultados a predecir y la métrica principal de cada uno.** Los modelos predicen valores
  esperados, así que la métrica tiene que premiar la media
  ([ADR 0004](../docs/decisions/0004-metricas-de-seleccion.md)): en WR, RMSE para recepciones y
  yardas, y deviance de Poisson para los touchdowns, un conteo con mayoría de ceros donde el MAE
  premia predecir cero.
- **Las variables.** Las 25 de WR no se trasladan; se eligen con el procedimiento del
  [ADR 0005](../docs/decisions/0005-seleccion-de-variables.md).
- **La configuración de cada modelo** (4.3 y 4.4).
- **Los límites de seguimiento.** Se calculan en 6.1 con el walk-forward de la posición, con
  `n_series` igual al número de resultados por tres métricas
  ([ADR 0006](../docs/decisions/0006-seguimiento-semanal.md)).

Se reutiliza sin volver a decidir, porque son reglas del proyecto:

- La partición temporal: entrenamiento 2016-2021, validación 2022-2023 y prueba 2024-2025. Se elige
  en validación y la prueba solo se reporta.
- Las diferencias con bootstrap por semanas completas, el ajuste de Bonferroni cuando se comparan
  varias alternativas y la regla de empates (ADR 0004).
- Los requisitos para que un modelo sea elegible (`experimentos.REQUISITOS`).
- El margen de no inferioridad de 1% para el tamaño del conjunto de variables (ADR 0005).
- La regla de alertas, el historial y la política de reentrenamiento (ADR 0006).

## 6. Punto de partida por posición

Propuesta para arrancar el análisis exploratorio de cada posición; cada resultado se confirma o se
descarta ahí. Las columnas son de `load_player_stats` y se definen en el
[glosario](../docs/data/GLOSARIO_VARIABLES.md) (secciones 5 y 6, y apéndice A).

| Posición | Resultados candidatos | Filtro | Lo que ya se sabe | A revisar |
|---|---|---|---|---|
| TE | `receptions`, `receiving_yards`, `receiving_tds`, igual que WR | `position == "TE"`; alineación: `TE` en los dos esquemas | Las estadísticas, las candidatas y `calcular_ratios_eficiencia` de WR aplican tal cual: es la adaptación más directa | Cuánto pesan las semanas sin producción, porque muchos TE juegan sobre todo para bloquear |
| RB | `carries`, `rushing_yards`, `rushing_tds`; por recepción, `receptions` y `receiving_yards` | `position == "RB"`; alineación: hasta 2024 aparecen como `RB`, `HB` o `FB`, y desde 2025 como `RB` | Los puntos de fantasy suman carrera y recepción, así que son dos grupos de variables | Qué códigos de alineación cuentan: `cargar_depth_charts_historico` filtra uno solo. Equipos que reparten los acarreos entre varios corredores |
| QB | `passing_yards`, `passing_tds`, `passing_interceptions`; por carrera, `rushing_yards` y `rushing_tds` | `position == "QB"`; alineación: `QB` | `data.identificar_qb_titular` identifica al titular de cada semana jugada (el de más intentos de pase) | En la semana por jugar todavía no hay intentos: el titular esperado sale de la alineación. Cómo tratar a los suplentes |

Los códigos de posición se verificaron en las estadísticas de 2024-2025 y en las alineaciones de
2024 (esquema anterior) y 2025 (esquema nuevo).

## 7. Antes de dar por terminada la posición

- Ningún notebook termina con error, y cada cifra de las lecturas sale de una salida ejecutada.
- 6.1 guarda los modelos y comprueba que, recargados como los carga el flujo, predicen lo mismo.
- Una semana pendiente corre de punta a punta, y `tracking.csv` tiene sus filas.
- Las variables y los resultados nuevos están en el glosario.
- Si alguna decisión se aparta de los ADR de WR, queda en un ADR propio.
