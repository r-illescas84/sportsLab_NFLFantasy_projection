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
empezó, no tiene uso ahora mismo) y **el foco es WR semanal, de principio a fin** — Ricky se hizo
cargo de las demás posiciones y usará este trabajo como guía cuando entregue su notebook. Ver
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

Feature engineering informado por Fase 3: 2 notebooks nuevos (`10_ingenieria_features.ipynb`,
`11_seleccion_features.ipynb`), 10 funciones nuevas en `features.py` + `identificar_qb_titular` en
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
