# Notebooks de WR semanal

Ordenados por etapa de un proyecto de ciencia de datos (CRISP-DM). El número `etapa.paso` indica
el orden de lectura. Cada notebook responde una pregunta; la lógica que se usa cada semana vive en
`../src/` y los notebooks solo la importan.

El planteamiento del problema y las decisiones de alcance están en
[`docs/PLAN.md`](../../../docs/PLAN.md) y en los ADR de
[`docs/decisions/`](../../../docs/decisions/). Las variables y términos se definen en el
[glosario](../../../docs/data/GLOSARIO_VARIABLES.md).

## 1. Datos

| Notebook | Pregunta |
|---|---|
| [`1.1_calidad_fuentes`](1.1_calidad_fuentes.ipynb) | ¿Se puede confiar en `nflreadpy` como fuente? |
| [`1.2_construccion_datos`](1.2_construccion_datos.ipynb) | ¿Qué hace `data.py` y por qué está construido así? |
| [`1.3_unificacion_alineaciones`](1.3_unificacion_alineaciones.ipynb) | ¿Cómo se unen las alineaciones de los dos esquemas de `nflreadpy`? |
| [`1.4_promedios_sin_fuga`](1.4_promedios_sin_fuga.ipynb) | ¿Los promedios del jugador usan solo partidos anteriores? |

## 2. Análisis exploratorio

| Notebook | Pregunta |
|---|---|
| [`2.1_eda_targets`](2.1_eda_targets.ipynb) | ¿Cómo se distribuyen los tres resultados que se predicen? |
| [`2.2_eda_perfil_variables`](2.2_eda_perfil_variables.ipynb) | ¿Qué forma tiene cada variable por sí sola? |
| [`2.3_eda_general`](2.3_eda_general.ipynb) | ¿Edad, experiencia, draft y contexto del partido se relacionan con los resultados? |
| [`2.4_eda_equipos`](2.4_eda_equipos.ipynb) | ¿Importa para qué equipo juega el receptor? |
| [`2.5_eda_multivariable`](2.5_eda_multivariable.ipynb) | ¿Cómo se relacionan las variables entre sí y con los resultados? |
| [`2.6_eda_jugador_destacado`](2.6_eda_jugador_destacado.ipynb) | Un jugador a fondo: Ja'Marr Chase |

## 3. Variables

| Notebook | Pregunta |
|---|---|
| [`3.1_ingenieria_features`](3.1_ingenieria_features.ipynb) | ¿Qué variables nuevas se construyen y cómo se verifica que no tienen fuga? |
| [`3.2_seleccion_features`](3.2_seleccion_features.ipynb) | ¿Qué variables entran al modelo? (primera selección) |
| [`3.3_seleccion_estable`](3.3_seleccion_estable.ipynb) | ¿Qué variables necesitan los modelos, con un orden estable y sin tocar la prueba? |

## 4. Modelado

| Notebook | Pregunta |
|---|---|
| [`4.1_preparacion_y_metricas`](4.1_preparacion_y_metricas.ipynb) | ¿Con qué tabla, particiones y métricas se comparan los modelos? |
| [`4.2_modelos_recepciones_yardas`](4.2_modelos_recepciones_yardas.ipynb) | ¿Qué modelo predice mejor recepciones y yardas? |
| [`4.3_modelos_touchdowns`](4.3_modelos_touchdowns.ipynb) | ¿Qué modelo predice mejor touchdowns, un conteo con mayoría de ceros? |
| [`4.4_ajuste_hiperparametros`](4.4_ajuste_hiperparametros.ipynb) | ¿Cuánto mejora un ajuste de hiperparámetros? |
| [`4.5_regresion_cuantiles`](4.5_regresion_cuantiles.ipynb) | ¿Se puede dar un piso y un techo (P10/P90) además del valor esperado? |
| [`4.6_ensamble_simple`](4.6_ensamble_simple.ipynb) | ¿Ayuda promediar varios modelos? |
| [`4.7_jerarquico_touchdowns`](4.7_jerarquico_touchdowns.ipynb) | ¿Un modelo jerárquico con Binomial Negativa mejora touchdowns? |
| [`4.8_jerarquico_recepciones_yardas`](4.8_jerarquico_recepciones_yardas.ipynb) | ¿Se repite en recepciones y yardas? |
| [`4.9_ventana_entrenamiento`](4.9_ventana_entrenamiento.ipynb) | ¿Conviene entrenar con menos años o dar más peso a los recientes? |

## 5. Evaluación del modelo elegido

| Notebook | Pregunta |
|---|---|
| [`5.1_estabilidad_temporal`](5.1_estabilidad_temporal.ipynb) | ¿El modelo elegido se mantiene año con año? |
| [`5.2_benchmark_vs_legado`](5.2_benchmark_vs_legado.ipynb) | ¿Mejora lo que reportaba el pipeline original? |
| [`5.3_explicabilidad_y_casos`](5.3_explicabilidad_y_casos.ipynb) | ¿Por qué predice lo que predice, dónde falla y dónde acierta? |

## 6. Modelo final y despliegue

| Notebook | Pregunta |
|---|---|
| [`6.1_modelo_final_y_temporada_actual`](6.1_modelo_final_y_temporada_actual.ipynb) | Entrenamiento con toda la historia, límites de seguimiento, guardado y validación con la temporada en curso |
| [`6.2_cierre`](6.2_cierre.ipynb) | Síntesis y tarjeta de cada modelo |
| [`6.3_seguimiento_semanal`](6.3_seguimiento_semanal.ipynb) | ¿Cómo se sabe cada semana si el modelo se está desviando, y cuándo se reentrena? |

El flujo semanal que aplica estos modelos se ejecuta con `weekly/wr/src/pipeline.py` (ver
[`weekly/README.md`](../../README.md) y [`docs/arquitectura/`](../../../docs/arquitectura/)). `_reference/` guarda los notebooks originales
del proyecto, congelados como referencia.
