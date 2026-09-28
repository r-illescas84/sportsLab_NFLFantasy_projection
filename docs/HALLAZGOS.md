# Hallazgos — pendientes para la fase de gobernanza

Cosas encontradas durante la Fase 1 (réplica) que no se arreglan ahora — se corrigen después,
en la fase de gobernanza. Cada entrada: qué es, dónde, por qué importa.

---

- [ ] **Notebook `weekly_stats.ipynb` duplicado** en dos rutas (`Weekly projections/WR/notebooks/`
  y `data/weekly_data/`), idéntico en lógica, solo cambia dónde guarda el CSV. La copia de
  `Weekly projections/WR/notebooks/` apunta a una carpeta ya marcada como "duplicado superado"
  en `.gitignore` (`Weekly projections/WR/data/`). Acción sugerida: quedarse con una sola copia
  (la de `data/weekly_data/`) y borrar la otra.

- [ ] **Predicciones semanales duplicadas** en dos carpetas idénticas byte a byte:
  `Weekly projections/WR/outputs/2025/` y `data/weekly_data/weekly_predictions/2025/`
  (confirmado con `diff` en `wrs_complete_week7.csv` y `wrs_rec_yds_pred_week_7.csv`). Acción
  sugerida: quedarse con una sola ubicación canónica.

- [ ] **`last5_avg.ipynb` truena en pandas 2.3.3 (celdas de equipo, no la de jugador)**: las celdas
  de `team_defense_last5_avg`/`team_offense_last5_avg` hacen
  `def_stats[[f"{c}_last5" for c in cols]] = def_stats.groupby(...).apply(...)` **sin**
  `.reset_index(drop=True)`. En el entorno original esto corrió bien (el CSV real existe), pero
  en pandas 2.3.3 lanza `TypeError: incompatible index of inserted column with frame index`.
  Nota relacionada: la celda de jugador del mismo notebook (y de `career_avg.ipynb`) sí tiene
  `.reset_index(drop=True)` — eso es justo lo que causa el bug de fuga de información ahí (ver
  bug ya documentado). O sea, la versión "sin bug" (equipo) es la que truena con pandas nuevo, y
  la versión "con bug" (jugador) es la que corre sin error.
  **Fix confirmado (probado en la réplica, coincide exacto — MAE=0.0000 — contra los reales):**
  agregar `.droplevel(0)` después del `.apply(...)`. La causa: en pandas nuevo, ese `.apply()`
  devuelve un `MultiIndex` (equipo, índice original) en vez de solo el índice original; con
  `.droplevel(0)` se recupera el índice original y la asignación alinea correctamente.

- [ ] **El mismo patrón "sin `reset_index`" (truena en pandas nuevo) se repite en `wrs_rec_yds.ipynb`,
  `wrs_receptions.ipynb` y `wrs_rec_tds.ipynb`** (celdas 10, 11, 31, 32 — agregados de equipo
  ofensivo/defensivo, histórico y "temporada actual"). Mismo fix confirmado: `.droplevel(0)`.

- [ ] **El bug de fuga de `career_avg` está duplicado inline** dentro de `wrs_rec_yds.ipynb` /
  `wrs_receptions.ipynb` / `wrs_rec_tds.ipynb` (celdas 13 y 34) — no solo vive en
  `career_avg.ipynb`. Arreglar el archivo compartido no arregla estos 3 notebooks; cada uno
  recalcula su propia versión del promedio de carrera desde cero.

- [ ] **Ruta de guardado con separador de Windows** en `wrs_rec_yds.ipynb` (y presumiblemente
  `wrs_receptions.ipynb`/`wrs_rec_tds.ipynb`): `f"weekly_predictions\{current_year}\..."` — solo
  funciona en Windows. En Linux/Mac no crea la carpeta esperada. Cambiar a `/`.

- [ ] **`wr_merge_stats.ipynb` tiene el mismo bug de ruta de Windows** en sus 2 celdas de
  lectura/escritura (`weekly_predictions\{current_year}\...`). Mismo fix: cambiar a `/`.

- [ ] **Desajuste doc/código en `wrs_rec_yds.ipynb`**: el markdown dice "WRs con al menos una
  temporada de 300+ yardas", el código usa `threshold = 400`.

- [ ] **La aplicación del modelo a la semana en curso depende de "hoy" (`datetime.today()`)** —
  trae el *depth chart* del día en que se corre y *play-by-play* de la temporada actual.
  Confirmado con evidencia real: al re-correr `wrs_rec_yds.ipynb` en 2026 para una semana de
  2025, el *depth chart* de "hoy" no tiene datos de esa temporada y la lista de titulares sale
  vacía, causando un error (`Columns must be same length as key`). El **entrenamiento** del
  modelo (datos históricos fijos 2014-2024) sí es reproducible — coincide bit por bit contra el
  original — pero la aplicación a una semana específica **no se puede reproducir después del
  hecho** sin guardar un snapshot histórico de esos datos "en vivo". Pendiente decidir si vale
  la pena empezar a guardar esos snapshots para que esto sea auditable a futuro.

- [ ] 🔴 **No existe código que arme los archivos `*_zama.csv` finales de Gen1.** Los notebooks
  de agregación (`agg_ricky.ipynb`, `ricky_general_metrics.ipynb`) producen columnas de ADP/
  metadata (`snap_share`, `apy_cap_pct`, `draft_pick`, `time_to_throw`...) que **no existen** en
  `qb_zama24.csv`. Las columnas reales del zama (`pass_attempts`, `pass_yards`, `pass_td`, etc.)
  no se generan ni se juntan en ningún notebook/script del repo — se buscó explícitamente con
  `grep` y no hay ninguna referencia a los CSVs que sí producen los scripts `.R`
  (`efficiencyQB_2024.csv`, `qb_run_share_2024.csv`, `redzone_statsQB_2024.csv`). El cruce final
  se hizo a mano, sin dejar rastro de código. Mapeo reconstruido manualmente (columna zama →
  script fuente) documentado en la bitácora de la Fase 1.

- [ ] 🔴 **`gsis_id` mal asignado — problema sistémico, no aislado.** En `qb_zama24.csv`, el id
  `00-0037834` aparece en 2 filas con nombres distintos ("Brian Robinson Jr.", RB, y "Brock
  Purdy", QB) — la fila de Robinson probablemente tiene pegadas las stats de pase de Purdy. En
  `te_zama24.csv` es peor: el id `00-0036244` aparece en **3 filas con 3 jugadores distintos**
  ("Colby Parkinson" TE, "Donta Foreman" RB, "Jeff Wilson Jr." RB), y uno de ellos tiene un
  valor de `Team` corrupto (`"RB79"`, no es un equipo real — parece una concatenación rota de
  posición+número de jersey). El origen es el archivo de ADP/gsis_id (`data/adp_full_gsispos_*.csv`
  o su antecesor), no algo introducido en `agg_ricky.ipynb`. Confirmado al reconstruir: la
  réplica hereda el mismo `gsis_id` incorrecto y reproduce el mismo duplicado. Vale la pena
  revisar cuántos casos más de este tipo hay en el archivo de origen antes de confiar en él
  para RB/WR.

- [ ] **`WR/` y `TE/` tienen los scripts de RB copiados sin adaptar** (`RAtt (1).R`,
  `RushYardsPG.R`, `Rush_Td.R`, `Fumbles.R`, `Targets.R`, `RecTotal.R`, `RecYrdsPG.R`,
  `Rec_Td.R`, `20+YrdPlay_Func.R` — los 9 son copias idénticas byte a byte, confirmado con
  `diff`). No es necesariamente incorrecto (un receptor puede tener acarreos ocasionales), pero
  confirma que las 3 carpetas parten de una plantilla sin limpiar ni adaptar nombres/comentarios.

- [ ] 🔴 **`RB/Targets.R` (`targets_jugador()`) truena si el jugador tiene 0 targets** — a
  diferencia de las demás funciones, no tiene rama especial para "sin datos"; el
  `data.frame(jugador=jugador, targets=total_targets)` final falla porque `jugador` queda
  vacío (0 elementos) mientras `targets=0` (1 elemento). Mismo archivo, además, corre código de
  uso automáticamente al cargarlo (`source()`) en vez de dejarlo comentado como el resto.

- [ ] 🔴 **Stats duplicadas (2x) para Tank Dell en `wr_completo24.csv`**: el archivo real muestra
  152 targets / 94 recepciones / 1418 yardas — casi exactamente el doble de mi reconstrucción
  (76 / 47 / 709). Las stats reales de Tank Dell en la temporada 2023 (su año de novato,
  truncado por lesión) fueron 47 recepciones / 709 yardas — coinciden con la réplica, no con el
  archivo real. Su `Team` también está corrupto (`"HU"`, no es una abreviación válida — debería
  ser `"HOU"`), lo que sugiere que esta fila se mezcló mal con otra durante el armado manual.

- [ ] **Las funciones `.R` de Gen1 regresan columnas distintas cuando el jugador no tiene
  jugadas de ese tipo.** Ej. `Rush_yrds_pg()` normalmente regresa `nombre/partidos/yardas/
  yardas_por_partido`, pero si el jugador tiene 0 acarreos regresa `id/rush_yards` — nombres de
  columna completamente distintos. Confirmado con Davis Cheek (`00-0037360`, QB, 0 jugadas de
  cualquier tipo en 2024) — rompe cualquier código que junte los resultados de varios
  jugadores esperando el mismo esquema. Fix: unificar el esquema de retorno en ambas ramas de
  cada función.

- [ ] 🔴 **`QB/Total_tds.R` tiene una ruta absoluta de Windows quemada al disco de una persona
  específica**: `source("C:/Users/HP15/Documents/NFL-Proyecto Fantasy/Terminadas/Pass_Td.R")`
  (y lo mismo para `Rush_Td.R`/`Rec_Td.R`). No corre en ninguna máquina que no sea esa —
  probablemente ni en la del propio autor si cambió de equipo. Fix: usar rutas relativas.

- [ ] **`wrs_xgboost.ipynb` (prototipo viejo, no genera archivo)**: calcula un filtro `wrs`
  (solo WR con volumen mínimo) pero entrena con `data_f[features]` sin filtrar — el filtro se
  calcula y nunca se usa. No bloquea nada porque este notebook no alimenta el pipeline real,
  pero conviene decidir si se borra o se corrige.

- [ ] **Columnas duplicadas en `team_offense_season_avg_2014-24.csv`**: `completions_season_avg`,
  `attempts_season_avg`, `carries_season_avg` y `receptions_season_avg` aparecen 2 veces cada
  una en el encabezado (confirmado leyendo el CSV real). Causa: en `season_avg.ipynb`, la lista
  `cols` usada para el cálculo de equipo ofensivo repite esos 4 nombres por error de copiado.
  No corrompe los valores (ambas copias traen el mismo dato), pero rompe cualquier acceso por
  nombre de columna (`df['carries_season_avg']` devuelve 2 columnas, no 1). Acción sugerida:
  quitar los nombres repetidos de la lista `cols`.
