# Plan de trabajo — WR semanal

Mapa de las fases de este trabajo y su estado. No repite lo que ya vive en otro documento:

- **Decisiones de arquitectura** → `docs/decisions/` (ADRs).
- **Defectos encontrados** → `docs/HALLAZGOS.md`.
- **Continuidad entre sesiones** (qué se hizo hoy, qué falta) → `docs/BITACORA.md`.
- **Este archivo**: qué cubre cada fase, en qué queda cuando se cierra, y el resultado concreto
  al que se llegó — para poder revisar el avance sin releer todo `BITACORA.md`.

## Regla de escalabilidad (aplica a todas las fases desde la Fase 2)

La lógica real vive en `.py` (funciones puras, parametrizables, importables desde cualquier
lado). Los notebooks **consumen** esas funciones para explorar/visualizar — no cargan lógica
propia que solo existe ahí. Esto es lo que permite, más adelante, orquestar el pipeline completo
sin depender de correr notebooks a mano uno por uno.

## Fases

| Fase | Qué se hace | Entregable | Estado |
|---|---|---|---|
| 1. Reorganización | Mover `annual/` (en pausa) y `weekly/wr/`; aislar el pipeline heredado como referencia congelada | Estructura de carpetas, 2 hallazgos de duplicidad cerrados | ✅ Cerrada 2026-09-28 |
| 2. Construcción de datos | Migrar a `nflreadpy` (mapeo verificado, consulta directa, cache nativo de la librería); un módulo de datos + uno de features en `.py`, sin triplicar; auditoría de calidad por fuente con criterio de acción | `weekly/wr/src/data.py`, `features.py`, sección nueva en el Datasheet + notebook de verificación | ✅ Cerrada 2026-09-29 |
| 3. EDA de WR | Explicar cada target con ejemplo real; distribución de los 3 targets; EDA general, por equipo, de un jugador destacado, multivariable sistemático, y perfil de forma de cada variable | 6 notebooks de EDA | ✅ Cerrada 2026-09-30 |
| 4. Feature engineering informado | Construir variables nuevas, evaluarlas todas (viejas y nuevas) en conjunto contra un modelo, seleccionar con evidencia | Lista de features con su justificación, en `features.py` | ✅ Cerrada 2026-09-30 |
| 5. Modelado comparado | Patrón de `03_modelo_predictivo` (Baseline/Ridge/RandomForest/XGBoost/HistGradientBoosting, split temporal), técnicas de conteo para touchdowns, estabilidad, SHAP, benchmark contra el legado, validación con 2026 | `modeling.py` + 8 notebooks (`12`-`19`) | ✅ Cerrada 2026-09-30 |
| 6. Estabilidad / ciclo de vida | `tracking.csv` por corrida; chequeo recurrente de las 6 dimensiones de calidad | Historial de métricas, visible si el modelo se degrada | Pendiente |
| 7. Documentar el patrón | Forma del pipeline en términos genéricos, para cuando llegue el notebook de Ricky | Guía corta de replicación en `weekly/README.md` | Pendiente |
| **Entregable final** | — | Predicción semanal real: punto + piso/techo, modelo ganador, frescura de datos | — |

---

## Registro por fase

### Fase 1 — Reorganización (cerrada 2026-09-28)

- `annual/` y `weekly/wr/` creados con `git mv` (historial conservado).
- Pipeline WR vivo identificado con certeza (`wr_merge_stats.ipynb` + 3 `wrs_rec_*.ipynb`) contra
  6 notebooks huérfanos sin consumidor real y una copia atrasada en `data/weekly_data/` —
  ambos retirados del control de versiones.
- Verificado (no asumido): las 6 llamadas de `nfl_data_py` que usa el pipeline tienen equivalente
  directo en `nflreadpy`, incluyendo `import_weekly_data` → `load_player_stats(summary_level="week")`
  (una primera revisión superficial decía que no existía; la referencia oficial de la API lo
  contradice).
- Notebooks heredados aislados en `weekly/wr/notebooks/_reference/`, congelados.
- Commits: `d1a6229`, `f09fb29`.

### Fase 2 — Construcción de datos (cerrada 2026-09-29)

- **2.1** Verificado en vivo: `load_player_stats(summary_level="week")` no comparte el rezago de
  `import_weekly_data()` — trae la temporada en curso al día (probado el mismo día, semana 3 de
  2026 completa disponible mientras `nfl_data_py` seguía dando 404).
- **2.4** Auditoría de calidad sobre 2 temporadas completas (2024-2025), las 5 fuentes:
  `load_players()` limpia (0 colisiones de `gsis_id`, contra el problema ya conocido del archivo
  de ADP); `load_schedules()`, `load_player_stats()` y `load_pbp()` mezclan playoffs por default
  (requieren filtrar `game_type`/`season_type`=="REG"); `load_depth_charts()` cambia de esquema
  completo entre 2024 y 2025, sin columna de temporada/semana compartida. Construyendo el
  adaptador aparecieron 3 trampas más de filtrado (ver `HALLAZGOS.md`). Verificación de tipos de
  dato: `birth_date`/`gameday` llegaban como texto en vez de fecha (corregido); otros casos de
  `float64` por nulos resultaron ser comportamiento esperado de pandas, no errores.
- **2.2 / 2.5** `weekly/wr/src/data.py` (6 funciones, 5 fuentes, cada filtro/conversión
  verificado) y `weekly/wr/src/features.py` (promedios de jugador sin fuga — verificado fila por
  fila con datos reales de DeVonta Smith; `target_share` confirmado nativo, ya no se recalcula a
  mano desde PBP como en el pipeline heredado).
- **2.6** 3 notebooks, cada uno respondiendo una pregunta propia, todos ejecutados de punta a
  punta sin errores: `01_calidad_fuentes.ipynb` (¿se puede confiar en `nflreadpy`?),
  `02_construccion_datos.ipynb` (`data.py` en acción, con las 3 trampas de `depth_charts`
  explicadas sobre el dato real que las reveló), `03_features.ipynb` (prueba de que no hay fuga).
- **Pospuesto a Fase 4, a propósito, no olvidado**: agregados de equipo (ofensiva/defensiva) y
  unificar `cargar_depth_charts_historico`/`_actual` en una sola función — solo si la evidencia
  muestra que valen la pena. La ventana `last3` vs `last5` es decisión de la Fase 3.
- Docker: `nflreadpy` + `pyarrow` instalados, cache nativo de la librería configurado
  (`nflreadpy_cache`, mismo patrón que `nfl_data_py`) — no se construyó un sistema propio de
  snapshots en CSV.

### Fase 3 — EDA de WR (cerrada 2026-09-30)

6 notebooks en `weekly/wr/notebooks/`, cada uno respondiendo una pregunta propia. **Reordenados
una vez** a mitad de la fase: el primer orden mezclaba univariado y multivariado sin criterio (un
notebook de "perfil de cada variable" quedó numerado *después* del de correlaciones, cuando debía
ir antes). El orden final sigue la secuencia metodológica correcta — univariado → bivariado →
multivariado → síntesis:

- **`04_eda_targets.ipynb`** (univariado — los targets): touchdowns es fundamentalmente distinto
  a recepciones/yardas — 82% en cero incluso sobre receptores activos (con ≥1 target esa semana).
- **`05_eda_perfil_variables.ipynb`** (univariado — todas las demás variables candidatas): con
  asimetría/curtosis/regla IQR formales — `racr` es numéricamente inestable (curtosis 241 en
  crudo, 605 al promediar — promediar lo empeora, no lo arregla); `carries`/`rushing_yards` no
  mejoran de forma ni promediados; 2 falsos positivos metodológicos documentados (touchdowns y
  `rest` marcan muchos "outliers" por IQR que en realidad son la forma esperada de un conteo raro
  o de un calendario con bye weeks/partidos de jueves, no errores de datos).
- **`06_eda_general.ipynb`** (bivariado — contexto vs. targets): la experiencia importa pero no
  linealmente — recepciones, yardas *y* touchdowns suben hasta 3-5 años de experiencia y bajan
  después (verificado en los 3, no solo yardas). `air_yards_share`/`wopr` fuera de [0,1] es
  aritmética real sobre un denominador pequeño, no error. Fuga verificada con evidencia (0.72
  circular → 0.47 real). Vegas/clima confirmado sin efecto, con código propio en el repo.
- **`07_eda_equipos.ipynb`** (bivariado — equipo vs. targets): ofensiva de equipo estable entre
  temporadas (correlación 0.40 2024→2025); dureza defensiva contra WR casi aleatoria entre
  temporadas (0.02) — primera evidencia real para la decisión de agregados de equipo pospuesta en
  Fase 2.
- **`08_eda_multivariable.ipynb`** (multivariado — todas las variables entre sí y contra los
  targets, sistemático): `draft_pick` correlaciona más que edad/experiencia (nunca antes
  considerado); un cambio de QB titular golpea la producción (~16% menos yardas, 594 casos) pero
  la calidad continua del QB casi no importa; `target_share`/`air_yards_share`/`wopr`
  correlacionadas 0.82-0.97 entre sí (redundantes para Ridge); acarreos de WR (jet sweeps) no
  predicen nada de su rendimiento como receptor; dureza defensiva confirmada sin efecto también
  dentro de la misma temporada y contra targets, no solo yardas entre temporadas.
- **`09_eda_jugador_destacado.ipynb`** (síntesis narrativa, cierre de la fase): Ja'Marr Chase,
  elegido por datos (mayor rango real entre receptores de volumen alto), con visualización de
  campo (`sportypy`) mostrando que su semana boom fue volumen+profundidad+acierto juntos, no una
  sola jugada de suerte.
- Todos los notebooks tienen referencias cruzadas "anterior/siguiente" consistentes con el orden
  final — no se dejaron hallazgos aislados sin reconciliar entre sí.

### Fase 4 — Feature engineering informado (cerrada 2026-09-30)

2 notebooks nuevos en `weekly/wr/notebooks/`: `10_ingenieria_features.ipynb` (construcción de
variables nuevas + verificación fila por fila de que ninguna tiene fuga) y
`11_seleccion_features.ipynb` (evaluación conjunta de todas las variables — viejas y nuevas —
contra un modelo simple, no solo correlación aislada, sobre 2016-2025 con split temporal
train 2016-2023/val 2024-2025).

- **Variables nuevas construidas** en `weekly/wr/src/features.py` (10 funciones) y `data.py`
  (`identificar_qb_titular`): ratios de eficiencia (`yards_per_target`, `catch_rate`,
  `air_yards_per_target`), `racr` acotado (winsorizado), volatilidad reciente, edad/años de
  experiencia con transformación no lineal, `draft_pick` (imputado para no drafteados), ofensiva
  de equipo, cambio de QB titular/de equipo, e interacciones.
- **`last3` vs. `last5` resuelto con evidencia de modelo, no solo correlación** (lo que
  `08_eda_multivariable.ipynb` dejó abierto a propósito): `last5` gana con claridad en los 3
  targets; `season_avg` es un complemento real; `career_avg` resulta el más débil una vez
  evaluado en conjunto, pese a su buena correlación aislada en Fase 3 — la misma lección de la
  edad en Fase 3, en dirección contraria.
- **`depth_team` (nunca antes evaluado) cierra ese hueco**: señal fuerte en recepciones/yardas,
  segundo lugar general en yardas — entra al feature set, con su límite de cobertura ya conocido
  (0% en 2025+ hasta unificar esquemas de depth chart).
- **Hallazgo honesto**: volatilidad reciente y las 3 interacciones propuestas (uso × ofensiva de
  equipo, draft × experiencia, cambio de QB × uso) no mostraron evidencia de aportar — probadas
  de buena fe, documentadas como intento sin confirmar, no incluidas.
- **`rest`/`div_game`** (calculados en Fase 3 pero nunca interpretados) y **cambio de QB
  titular/equipo** (aporte marginal casi nulo una vez presentes las features de uso reciente) no
  entran al feature set principal — matiza, no invalida, el hallazgo bivariado de Fase 3 sobre
  cambios de QB.
- Bug real encontrado al construir el cruce de depth chart: `cargar_depth_charts_historico()`
  podía regresar 2 filas para el mismo jugador-semana con `depth_team` distinto (357 de 27,928
  casos) — corregido en `data.py`, documentado en `HALLAZGOS.md`.
- Tabla completa `feature → decisión → evidencia` en el docstring de módulo de
  `weekly/wr/src/features.py` (entregable de esta fase).
- **Pospuesto a Fase 5, a propósito**: si entrenar con todas las semanas o solo con las activas
  (`targets > 0`) es una decisión de función de pérdida (Poisson/Tweedie), no de qué columnas
  usar — no se resuelve aquí.

### Fase 5 — Modelado comparado (cerrada 2026-09-30)

8 notebooks nuevos (`12` a `19`) y un módulo nuevo `weekly/wr/src/modeling.py` (split temporal,
bake-off de modelos, walk-forward, modelo de 2 etapas para conteos). `features.py` gana
`construir_tabla_modelado()` (une todo el bloque de Fase 4 en una función reusable, con soporte
para filas "placeholder" de una semana futura) y `columnas_por_modelo()` ahora regresa el feature
set completo (34 columnas árbol / 30 Ridge), no solo el subconjunto de colinealidad.

- **Split de 3 particiones, distinto al de Fase 4 y por qué**: train 2016-2021 / val 2022-2023
  (elige modelo) / test 2024-2025 (se mira una sola vez) — Fase 4 usaba 2 particiones porque solo
  hacía *screening* de features con un modelo fijo; aquí se elige entre familias de modelo e
  hiperparámetros, y reusar el mismo val para ambas decisiones sería fuga de decisión.
- **XGBoost gana en los 3 targets** (`13_comparacion_modelos.ipynb`, `14_receiving_tds_modelos_de_conteo.ipynb`):
  MAE test 1.372 (recepciones), 20.647 (yardas), 0.2466 con objetivo `reg:tweedie` (touchdowns,
  probado contra Poisson/Tweedie/HGB-Poisson/hurdle — ganó con ventaja clara, aunque con R² más
  negativo que el resto, documentado sin ocultar).
- **Decisión de Fase 4 resuelta con evidencia**: entrenar con todas las semanas gana sobre filtrar
  activas, en los 3 targets y los 5 modelos (comparación con el mismo conjunto de prueba activo en
  ambos casos, para que sea justa).
- **Estabilidad confirmada 2 veces**: walk-forward 2021-2025, coeficiente de variación 4.5%-4.7%
  en los 3 targets; validación adicional contra 2026 (dato que no existía en ningún momento
  anterior del proyecto) en el mismo rango de MAE, sin degradarse.
- **Benchmark real contra el legado** (`16_benchmark_vs_legado.ipynb`): mejora de MAE de 4.5%
  (recepciones), 7.2% (yardas) y 17.2% (touchdowns) sobre las mismas 929 predicciones reales que
  el legado ya guardó para las semanas 7-18 de 2025, comparadas contra el resultado real de esas
  semanas — no una simulación aparte.
- **Bug real encontrado y corregido en el camino**: los CSVs de `weekly/wr/outputs/2025/`
  (`*_pred_week_N.csv`) tienen filas duplicadas por jugador-semana con predicciones distintas
  entre sí (hasta 7.6x en `wrs_complete_week18.csv`) — auditado y documentado en `HALLAZGOS.md`,
  resuelto con una regla explícita de promediar antes de calcular el benchmark.
- **Explicabilidad** (`17_explicabilidad_shap.ipynb`): SHAP coincide con permutación (Fase 4) e
  importancia nativa de árbol en las variables más influyentes. Hallazgo honesto y repetido 3
  veces de forma independiente (cuartil de uso, caso real de Ja'Marr Chase, los 5 peores errores):
  **el modelo subestima sistemáticamente las semanas boom** — mismo patrón que ya documentó Daniel
  en `preeliminar/03_modelo_predictivo`.
- **Predicción real generada** (`18_validacion_temporada_actual.ipynb`): usando el estado real de
  hoy (`cargar_depth_charts_actual`) se predijo la semana 4 de 2026, todavía sin jugarse — ranking
  encabezado por WR1 reconocibles, con su frescura de datos documentada.
- **Los 3 modelos ganadores quedan guardados** en `weekly/wr/models/` (`{target}_xgboost.json` +
  `{target}_metadata.json` con features, hiperparámetros, métricas y limitaciones conocidas) —
  entrenados con toda la historia 2016-2025, verificados con una recarga desde disco que reproduce
  predicciones idénticas.
- **Pospuesto a propósito, no en silencio**: Model Cards formales, `tracking.csv` y regresión de
  cuantiles quedan para Fase 6/7 — esta ronda fue "primeras pruebas", no la infraestructura
  recurrente de producción.

### Fase 5.1 — Mejoras de bajo costo post-cierre (cerrada 2026-09-30)

3 notebooks nuevos (`21` a `23`), sin búsqueda de hiperparámetros ni entrenamientos pesados a
propósito — igual de rigurosos, pero deliberadamente baratos de correr.

- **`21_unificacion_depth_chart.ipynb`**: resuelve la limitación de `depth_team` 100% nulo en
  2025+ (ver `HALLAZGOS.md`, cerrado). `data.cargar_depth_charts_unificado()` mapea cada scrape
  del esquema nuevo a su próximo partido real (`pd.merge_asof`, verificado con Marvin Harrison
  Jr. y Ja'Marr Chase). Cobertura 0% → ~97% en 2025. Impacto real, medido con los mismos
  hiperparámetros ya elegidos (sin buscar nada nuevo): mejora modesta pero consistente de R² en
  los 3 targets; MAE mejora en recepciones, neutro en yardas, ligeramente peor en touchdowns
  (mismo patrón MAE-vs-R² ya conocido de ese target). Los 3 modelos base se reentrenaron con el
  dato corregido y quedan guardados como la nueva referencia.
- **`22_regresion_cuantiles.ipynb`**: agrega P10/P50/P90 a los 3 targets
  (`HistGradientBoostingRegressor(loss="quantile")`, preferido sobre el objetivo nativo de
  cuantiles de XGBoost con evidencia real — mejor pinball loss, 0% de cuantiles cruzados vs ~4%,
  mejor cobertura empírica). `receptions`/`receiving_yards` logran cobertura ~80-82% como se
  espera; en `receiving_tds` (82% de semanas en cero) P10/P50 salen en 0 siempre — el rango solo
  aporta señal en P90 para ese target. Hallazgo adicional: el cuantil 0.5 (que optimiza MAE
  directo) le gana en MAE a los 3 modelos guardados pero empeora R² en los 3 — confirma que la
  tensión MAE-vs-R² no es exclusiva de touchdowns.
- **`23_ensamble_simple.ipynb`**: promediar los 3 modelos de árbol (XGBoost/RandomForest/
  HistGradientBoosting) no ayuda — están demasiado correlacionados entre sí (0.97-0.99) para que
  un ensamble reduzca error. Agregar Ridge (el único realmente diverso, ~0.93-0.95 de
  correlación) sí mejora MAE (~0.8-1.5%) y R² en recepciones/yardas. Para touchdowns, mezclar el
  XGBoost Tweedie con el modelo hurdle (correlación 0.62, mucho más diversos) sí tiene sentido:
  da un trade-off MAE-vs-R² ajustable con un solo parámetro, en vez de 2 extremos fijos — una
  opción concreta, nueva, para la decisión pendiente sobre ese target.
- **Todos los artefactos nuevos** (modelos de cuantil, componentes del ensamble) quedan en
  `weekly/wr/models/` junto a los 3 modelos base — ninguno fue reemplazado, son una capa
  adicional.
