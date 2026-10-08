# Bitácora de mantenimiento

Registro de continuidad entre sesiones de trabajo — no es documentación del proyecto (eso va
en los `README.md`). Una entrada corta por sesión: qué se hizo, qué falta.

---

## 2026-09-27

- Rama de trabajo creada: `daniel`.
- Confirmado que `main` es el estado más actualizado del repo.
- Creada la documentación base: este archivo y `docs/decisions/` (ADRs).
- Docker Desktop verificado corriendo (27.5.1 / Compose v2.32).
- Versiones de `scikit-learn`, `xgboost`, `joblib`, `shap` confirmadas y fijadas en
  `docker/requirements.txt`.
- Imágenes construidas (`nflfantasy-python`, `nflfantasy-r`) y verificadas: imports de Python
  y librerías de R cargan bien, y sobreviven a destruir/recrear el contenedor.
- `renv.lock` generado (120 paquetes). Se corrigieron dos fallas reales en `Dockerfile.r`: el
  caché de `renv` no persistía entre contenedores, y el CRAN grabado en el lockfile era un
  snapshot viejo sin la versión de `renv` necesaria.

**Pendiente:** primer commit en la rama `daniel`.

---

## 2026-09-28

- Primer commit hecho en `daniel` y rama pusheada a GitHub.
- ADR `0001-criterios-de-replica.md`: umbrales numéricos de éxito para la réplica.
- Fase 1 (Gen2, Python) completa — 7 notebooks: `weekly_stats`, `season_avg`, `career_avg`,
  `last5_avg`, `player_shares`, `wrs_rec_yds`/`wrs_receptions`/`wrs_rec_tds`, `wr_merge_stats`.
  Todos coinciden contra los archivos reales (exacto o dentro del umbral del ADR). El bug de
  fuga conocido se reprodujo igual (92.0% = 92.0%). 14 hallazgos documentados en
  `docs/HALLAZGOS.md`, con fixes ya probados para los más importantes.
- Fase 1 (Gen1, R) arrancada con QB como muestra (`qb_zama24.csv`, temporada 2023). Hallazgo
  importante: no existe código en el repo que arme los archivos `*_zama.csv` finales — se
  reconstruyó con un script nuevo (`_fase1_replica/gen1_qb/reconstruir_qb_zama24.R`) que
  reutiliza las funciones `.R` originales sin modificarlas. Coincide casi exacto (12/13
  columnas MAE=0.0000). 3 hallazgos nuevos: `gsis_id` mal asignado (Brian Robinson Jr. /
  Brock Purdy), esquema inconsistente en las funciones `.R` cuando un jugador no tiene
  jugadas, y ruta absoluta de Windows quemada en `Total_tds.R`.

Repetido el mismo ejercicio para RB (`rb_zama24.csv`, 87 jugadores), TE (`te_zama24.csv`, 42) y
WR (`wr_completo24.csv`, 111) — todos temporada 2023. Los 3 coinciden exacto contra los
archivos reales (RB necesitó sumar `pass_td` a `total_touchdowns`; WR tiene 1 jugador con datos
duplicados en el archivo real, no en la réplica). Confirmado: los 9 scripts `.R` de soporte
(`RAtt (1).R`, `RushYardsPG.R`, `Rush_Td.R`, `Fumbles.R`, `Targets.R`, `RecTotal.R`,
`RecYrdsPG.R`, `Rec_Td.R`, `20+YrdPlay_Func.R`) son copias idénticas entre QB/RB/WR/TE. 7
hallazgos nuevos documentados (mal mapeo de `gsis_id` en QB y TE, datos duplicados en WR,
`targets_jugador()` truena sin datos, scripts copiados sin adaptar).

**Fase 1 completa (Gen1 + Gen2).** Scripts de reconstrucción en `_fase1_replica/gen1_{qb,rb,te,wr}/`.
ADR 0001 + `HALLAZGOS.md` (18 hallazgos) commiteados y pusheados.

---

Replanteado el rumbo: no ir directo al modelo — primero entender los datos a fondo (industria +
evidencia propia), como un producto de datos, no solo un modelo. Investigación con fuentes
(TDSP, Datasheets/Model Cards, metodología de PFF, líneas de Vegas y EPA como señales
predictivas) documentada en el plan. Verificado contra los datos reales: `spread_line`/
`total_line` (Vegas) con 100% de cobertura 2016-2023, y EPA/`target_share`/`wopr` ya disponibles
en `import_weekly_data()` — ninguna de las dos se usa hoy en el pipeline.

Arrancada la Fase 2 (entendimiento de datos): `docs/data/DATASHEET.md` con auditoría de las 6
dimensiones de calidad, y ADR `0002-fuente-de-datos.md` (se sigue con `nflverse`, sin scouting
propietario por ahora). Escaneo sistemático de los 4 archivos finales (278 jugadores): 7 casos
reales de corrupción en `Team` (2.5%, patrón `HOU`→`"HU"`/`NO`→`"N"`) + 12 de convención distinta
pero válida (`JAC`/`LAR`) — 2 hallazgos nuevos agregados a `HALLAZGOS.md`.

**Pendiente:** commit de esto (Datasheet + ADR 0002 + hallazgos nuevos). Después: Fase 3 (EDA +
feature engineering informado por el Datasheet).

---

EDA de la Fuente 3 (archivo de ADP): confirmado que el cruce manual de `gsis_id` falla
silenciosamente para jugadores reales en 4 de 11 años (se mezclan con el marcador de defensas),
colisiones de identidad confirmadas en 2018/2019/2024 (no solo el caso ya conocido), y
profundidad de ranking inconsistente entre años (2019 rankea hasta #1047, el resto #290-488). 3
hallazgos nuevos + Datasheet actualizado, commiteados y pusheados.

## 2026-09-28 (tarde) — Alcance redefinido por el equipo

El equipo revisó el avance y redefinió el alcance: **lo anual queda en pausa** (la temporada ya
empezó, no tiene uso ahora mismo) y **el foco es WR semanal, de principio a fin** — otro integrante del
equipo se hizo cargo de las demás posiciones y usará este trabajo como guía. Ver
ADR `0003-alcance-wr-semanal.md`.

Verificado (no asumido) que `nflreadpy` sí tiene equivalente directo para las 6 funciones de
`nfl_data_py` que usa el pipeline de WR — incluyendo `import_weekly_data` (`load_player_stats`
con `summary_level="week"`), que una primera revisión superficial había marcado como "sin
equivalente". Pendiente de verificar en vivo si comparte el mismo rezago ya documentado.

Reorganización ejecutada:
- `QB/`, `RB/`, `TE/`, `WR/`, `DST/`, `K/`, `Rookies/`, `Models/`, `extras/`, `renv/`,
  `renv.lock`, `.Rprofile` → `annual/` (con `git mv`, historial conservado).
- Pipeline WR vivo (`Weekly projections/WR/notebooks/{wr_merge_stats,wrs_rec_tds,wrs_rec_yds,
  wrs_receptions}.ipynb` + `outputs/2025/`) → `weekly/wr/`.
- Retirados del control de versiones: `data/weekly_data/` completo (copia atrasada, confirmado
  con la reorganización) y los 6 notebooks huérfanos sin consumidor real (`career_avg`,
  `last5_avg`, `season_avg`, `player_shares`, `weekly_stats`, `wrs_xgboost`) — resuelve 2
  hallazgos de duplicidad ya documentados.
- READMEs nuevos en `weekly/`, `annual/`, y raíz actualizado con la estructura completa.

**Pendiente:** commit de la reorganización. Después: Fase 2 del nuevo plan — construcción de
datos (migrar a `nflreadpy`, un solo módulo de features en vez de triplicado).

## 2026-09-29 — Fase 2 completa

Construcción de datos sobre `nflreadpy`, con verificación real en cada paso (no solo "ya
funciona"). Detalle completo del avance y los hallazgos en `docs/PLAN.md` (Registro por fase) y
`docs/HALLAZGOS.md` — aquí solo el resumen de continuidad:

- `weekly/wr/src/data.py` (6 funciones, 5 fuentes) y `weekly/wr/src/features.py` (promedios de
  jugador sin fuga, verificado fila por fila) — nuevos.
- 3 notebooks nuevos en `weekly/wr/notebooks/`, cada uno ejecutado de punta a punta.
- Docker actualizado: `nflreadpy` + `pyarrow`, cache nativo configurado.
- Datasheet y `HALLAZGOS.md` actualizados con los hallazgos de esta fase (rezago, esquema roto
  de `depth_charts`, trampas de filtrado, tipos de dato).

Todo este bloque se subió junto en un solo corte, a propósito — se acordó no comitear cada paso
suelto de esta fase para no llenar el historial de commits pequeños.

**Pendiente:** Fase 3 — EDA de WR (empezar por explicar cada target con un ejemplo real, decidir
`last3` vs `last5` con evidencia).

## 2026-09-30 — Fase 3 completa

EDA de WR, 6 notebooks en `weekly/wr/notebooks/` (`04` a `09`). Detalle completo y hallazgos en
`docs/PLAN.md` (Registro por fase) — aquí solo el resumen de continuidad:

- Primer intento (4 notebooks) guiado por hipótesis puntuales, no sistemático — se corrigió con
  un notebook de revisión multivariable sobre **todas** las columnas de las 5 fuentes (150+39+46),
  no solo las ya conocidas.
- Ese mismo notebook multivariable había quedado mal ordenado (revisaba relaciones entre
  variables antes de revisar cada variable por sí sola) — se reordenaron y renombraron los 6
  notebooks para seguir la secuencia correcta: univariado → bivariado → multivariado → síntesis.
  Las introducciones y referencias cruzadas de cada uno se reescribieron para que coincidan con
  el orden final, no solo se renombraron los archivos.
- Hallazgos que cambian decisiones de Fase 4: `draft_pick` es mejor predictor que edad/experiencia;
  `last3` es consistentemente la ventana más débil de las 4 en los 3 targets (no solo yardas);
  `racr` es numéricamente inestable (no solo débil en correlación); la dureza defensiva del rival
  no aporta en ninguna forma de medirla; `features.py` ahora calcula `last3` y `last5` juntas, sin
  decidir cuál usar de antemano.
- `weekly/wr/src/features.py` actualizado (ambas ventanas) y su notebook de verificación
  reejecutado para reflejarlo.

Todo el bloque se sube junto, mismo criterio que la Fase 2.

## 2026-09-30 — Fase 4 completa

Feature engineering informado por Fase 3: 2 notebooks nuevos (`3.1_ingenieria_features.ipynb`,
`3.2_seleccion_features.ipynb`), 10 funciones nuevas en `features.py` + `identificar_qb_titular` en
`data.py`. Detalle completo del registro en `docs/PLAN.md`.

- Resuelto con evidencia de modelo (no solo correlación): `last5` gana en los 3 targets,
  `season_avg` complementa, `career_avg` resulta redundante en conjunto pese a buena correlación
  aislada en Fase 3.
- `depth_team` (nunca antes evaluado) aporta señal fuerte — cierra ese hueco de Fase 3.
- Volatilidad reciente y 3 interacciones propuestas, probadas de buena fe: sin evidencia de
  aportar, no incluidas. `rest`/`div_game` y cambio de QB/equipo: mismo resultado.
- Bug real encontrado y corregido: `cargar_depth_charts_historico()` podía duplicar filas
  jugador-semana con `depth_team` distinto (357 de 27,928 casos) — ver `HALLAZGOS.md`.

**Pendiente:** Fase 5 — modelado comparado (Baseline/Ridge/RandomForest/XGBoost, split temporal,
Model Card por target), incluyendo decidir ahí si entrenar con todas las semanas o solo las
activas (función de pérdida, no de features).

## 2026-09-30 — Fase 5 completa

Modelado comparado, con 8 preguntas explícitas cubiertas (detalle completo en `docs/PLAN.md`): 8
notebooks nuevos (`12` a `19`) y `weekly/wr/src/modeling.py` nuevo.

- XGBoost gana en los 3 targets (con objetivo `reg:tweedie` en `receiving_tds`, probado contra
  Poisson/Tweedie/HGB-Poisson/hurdle — sí se ejecutaron, no solo se mencionaron).
- Resuelto con evidencia: entrenar con todas las semanas gana sobre filtrar activas, en los 3
  targets — cierra la decisión pospuesta de Fase 4.
- Estabilidad confirmada 2 veces: walk-forward 2021-2025 (CV 4.5%-4.7%) y validación contra 2026
  real, en el mismo rango, sin degradarse.
- Benchmark real contra el legado: mejora de MAE 4.5%/7.2%/17.2% (recepciones/yardas/TDs) sobre
  las mismas predicciones reales de 2025 que el legado ya guardó, comparadas contra el resultado
  real de esas semanas.
- Bug real encontrado en el camino: los CSVs de predicciones 2025 del legado tienen filas
  duplicadas por jugador-semana (hasta 7.6x) — ver `HALLAZGOS.md`.
- Predicción real generada para la semana 4 de 2026 (todavía sin jugarse), con ranking de WR1
  reconocibles y frescura de datos documentada.
- Hallazgo honesto repetido 3 veces: el modelo subestima sistemáticamente las semanas boom.

**Pendiente:** Fase 6 (estabilidad/ciclo de vida: `tracking.csv`, checklist de calidad recurrente)
y Fase 7 (documentar el patrón para las demás posiciones) — a definir cuándo arrancar.

---

## 2026-09-30 — Fase 5.1 completa (mejoras de bajo costo)

3 notebooks nuevos (`21` a `23`), sin búsqueda de hiperparámetros ni entrenamientos pesados a
propósito.

- Unificado el esquema de depth chart 2024 vs. 2025+ (`data.cargar_depth_charts_unificado()`) —
  cobertura de `depth_team` en 2025 sube de 0% a ~97%. Cierra el hallazgo correspondiente en
  `HALLAZGOS.md`. Los 3 modelos base se reentrenaron con el dato corregido y quedan guardados
  como la nueva referencia.
- Agregada regresión de cuantiles (P10/P50/P90) a los 3 targets — cierra el punto que quedó
  pospuesto al cierre de Fase 5.
- Evaluado un ensamble simple: promediar solo los 3 modelos de árbol no ayuda (demasiado
  correlacionados); agregar Ridge sí mejora MAE/R² en recepciones/yardas. Para touchdowns,
  mezclar Tweedie con el modelo hurdle da un trade-off MAE-vs-R² ajustable — opción nueva para la
  decisión pendiente sobre ese target.
- Todos los artefactos nuevos quedan en `weekly/wr/models/` junto a los 3 modelos base, sin
  reemplazar ninguno.

**Pendiente:** decidir qué hacer con `receiving_tds` (R² sigue negativo en el modelo puntual;
la mezcla con hurdle del notebook 4.6 es una opción, no una decisión tomada). Fase 6/7 sin
arrancar.

---

## 2026-09-30 (continuación) — Exploración de modelos más explicativos

Se pidió explorar alternativas "más explicativas, sin modelos de caja negra, que se ajusten
mejor a los datos" — ninguno de los modelos probados hasta ahora explota que son los mismos
jugadores repetidos temporada tras temporada (dato de panel). Empezamos con un modelo jerárquico
(efectos mixtos) + Binomial Negativa para `receiving_tds`.

- Nueva herramienta en el proyecto: `pymc`/`bambi`/`arviz`/`h5netcdf` (Python, Bayesiano) —
  agregados a `docker/requirements.txt`, imagen reconstruida. Primera vez que `weekly/` usa algo
  más allá de `pandas`/`scikit-learn`/`xgboost`.
- `4.7_jerarquico_touchdowns.ipynb`: efecto aleatorio por jugador + Binomial
  Negativa da el **mejor R² de todos los intentos** para `receiving_tds` (+0.073, supera al
  hurdle) con MAE razonable. Totalmente interpretable sin SHAP (coeficientes con intervalos de
  credibilidad, efecto individual legible por jugador — ej. Ja'Marr Chase ~+30% sobre un jugador
  promedio con sus mismas features).
- Bug real encontrado y corregido: un `dropna()` ingenuo hubiera perdido 20.8% de las filas de
  entrenamiento (justo las primeras apariciones de temporada/carrera de cada jugador) — se
  imputa en vez de descartar. Otro bug real: `idata.to_netcdf()` falla al recargar
  (incompatibilidad `h5netcdf`/`xarray`) — resuelto con `joblib`, ver `HALLAZGOS.md`.
- Hallazgo honesto, contrario a la hipótesis inicial: el shrinkage no ayuda más a jugadores ya
  vistos en entrenamiento que a jugadores nuevos — de hecho los nuevos salen con mejor MAE.
- También se señaló que la ventana de evaluación (2016-2025 completo) puede ser demasiado
  larga — jugadores/equipos cambian de forma de juego año con año. Anotado para retomar, no
  resuelto todavía.

**Pendiente (al momento de escribir esto):** extender el mismo enfoque jerárquico a
`receptions`/`receiving_yards`; revisar la ventana de entrenamiento/evaluación — ver cierre abajo, ambos puntos ya resueltos con evidencia (aunque negativa en los 2
casos).

---

## 2026-09-30 (cierre) — Ventana de evaluación revisada, jerárquico extendido a los 3 targets

`4.9_ventana_entrenamiento.ipynb` y `4.8_jerarquico_recepciones_yardas.ipynb` cierran los 2
pendientes de la entrada anterior — ambos con resultado negativo, documentado tal cual, no
forzado.

- **Ventana de evaluación**: se confirma con evidencia real que el nivel de un jugador decae
  gradualmente con los años (correlación de `target_share` cae de 0.80 a 1 año a 0.58 a 5 años;
  `receiving_yards`/`receptions` de ~0.72 a ~0.50). Pero ninguna de las 2 correcciones probadas
  mejora el modelo: una pendiente temporal por jugador empeora MAE y R² (y la convergencia);
  acortar la ventana de entrenamiento (2019-2021 vs. 2016-2021) da MAE igual pero peor R². Lectura
  más probable: las 30 variables rezagadas que ya alimentan al modelo absorben la mayor parte de
  "qué tan vigente está" un jugador — el diseño actual se mantiene sin cambios.
- **Jerárquico en `receptions`/`receiving_yards`**: en ambos, el modelo jerárquico queda **por
  debajo** del XGBoost ya guardado (recepciones: MAE 1.48 vs. 1.35, R² 0.36 vs. 0.45; yardas: MAE
  21.3 vs. 20.4, R² 0.14 vs. 0.37). La mejora en `receiving_tds` no era "jerárquico es mejor en
  general" — era corregir una discrepancia real de distribución (Tweedie mal especificado para un
  conteo con 82% de ceros). Donde XGBoost ya se ajustaba bien, la estructura más rígida de un
  modelo lineal pierde contra un árbol.
- Bug real encontrado en el camino: 67 filas de `receiving_yards` son negativas (jugadas
  tackleadas detrás de la línea) — rompe un `log1p` directo. Se usa una transformación
  logarítmica con signo en su lugar.

**Recomendación con evidencia**: adoptar el jerárquico solo para `receiving_tds`; mantener
XGBoost sin cambios para los otros 2 targets. Sigue pendiente decidir si el jerárquico reemplaza
o complementa al Tweedie/hurdle para `receiving_tds` — no se tomó esa decisión todavía.

---

## 2026-10-01 — Fase 5.3 completa (arquitectura semanal)

Se corrigió el diseño de `modeling.py`: debía ser el paso del flujo semanal que aplica los
modelos ya evaluados y trae métricas, no la caja de herramientas de comparación.

- Reestructura de `weekly/wr/src/`: lo que corre cada semana (`data`, `features`, `modeling`,
  `pipeline`) separado de lo exploratorio (`experimentos`, `features_exploratorio`). Detalle y
  verificación en `docs/PLAN.md` (Fase 5.3).
- `pipeline.py` corrido sobre datos reales: semana 3 de 2026 (jugada, fuera de muestra) con
  métricas por debajo de las de referencia en recepciones y yardas, y semana 4 (pendiente) con 198
  WR predichos.
- Decidido: los modelos de cuantil y el jerárquico quedan fuera del flujo semanal.
- Corregida una referencia colgada en `HALLAZGOS.md` (el bug de recarga de XGBoost no estaba
  documentado). Hallazgo nuevo: el scrape de la mañana del partido se asigna a la semana siguiente
  y el flujo ahora lo detecta.

**Pendiente:** republicar la página de arquitectura; Fase 6 (`tracking.csv`) y Fase 7 sin arrancar;
sigue abierta la decisión de touchdowns (Tweedie vs. jerárquico).

---

## 2026-10-07 — Fase 5.4 completa (métricas de selección y orden por etapa)

Se revisó con la literatura si MAE era la métrica correcta para elegir modelos. No lo es: los modelos
predicen valores esperados y el MAE premia la mediana, que en touchdowns es cero. Decisión en el
ADR `0004-metricas-de-seleccion.md`; detalle y resultados en `docs/PLAN.md` (Fase 5.4).

- Política de métricas en `modeling.py` y `experimentos.py`: RMSE en recepciones y yardas, deviance de
  Poisson en touchdowns, elección en validación, R² fuera de muestra, requisitos de calibración y
  regla de empates por bootstrap.
- Notebooks reordenados por etapa (`etapa.paso`) y etapas 4 a 6 re-ejecutadas con la política nueva.
  El modelo de touchdowns pasa a XGBoost con objetivo Poisson: R² fuera de muestra de 0.08 en
  validación, contra −0.03 del anterior en prueba.
- Modelos del flujo semanal reentrenados con 2016-2025 (`6.1`) y salidas de 2026 regeneradas (semanas
  1-4 jugadas y semana 5 pendiente).
- Corregidos en el camino: el «techo» de R² de touchdowns en 4.1 era una referencia bajo Poisson, no
  un tope; la comparación del jerárquico de yardas no corregía la retransformación logarítmica; el
  CSV semanal de métricas no se redondeaba.

- La corrección por comparaciones múltiples (Bonferroni) quedó como regla en el ADR 0004 y en 4.1, a
  raíz de 4.9. El jerárquico de touchdowns también se probó mezclado con el XGBoost: el mejor peso
  (90/10) empata.
- Error en los datos: las alineaciones históricas usaban códigos de equipo de la época (`OAK`, `SD`)
  y traían una semana de más en 2016-2020. Corregido en `data.py`; se re-ejecutaron 1.3 y de 4.1 a
  6.2, con los modelos y las salidas de 2026. Cambió la configuración de touchdowns dentro de la
  grilla de 4.3 (200 árboles y tasa 0.08) y, en 5.2, la mejora en recepciones contra el flujo
  heredado pasa a empate con el intervalo ajustado.

- `3.2_seleccion_features.ipynb` re-ejecutado con el dato corregido: su regla daría 31 variables en
  lugar de las 34 que usan los modelos. Se mantienen las 34; la inestabilidad queda en `HALLAZGOS.md`.

- El total implícito de las líneas de apuestas se calculaba con la línea invertida en
  `2.3_eda_general.ipynb`; corregido, correlaciona 0.088 con las yardas (antes 0.0018). Queda como
  candidata para la selección de variables.

- `4.6_ensamble_simple.ipynb` deja de guardar los modelos de las alternativas que no se adoptan
  (componentes de los ensambles y modelo de dos etapas, 6.3 MB); queda solo su metadata. El
  jerárquico de 4.7 se guarda solo como insumo local de 4.9, sin versionar.
- Limpieza de texto de todos los notebooks (1.1 a 6.2), con cada cifra verificada contra las
  salidas. En 5.3 la comparación de SHAP usaba el orden de permutación de la corrida anterior de
  3.2; en 6.1, yardas y touchdowns de 2026 quedan un poco por encima de las referencias en la
  métrica principal, no dentro del rango.

**Pendiente:** rehacer la selección de variables con un método estable (incluye evaluar el total implícito) (métrica principal,
validación 2022-2023 y revisión de estabilidad); Fase 6
(`tracking.csv`) y Fase 7.
