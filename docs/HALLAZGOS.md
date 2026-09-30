# Hallazgos — pendientes para la fase de gobernanza

Cosas encontradas durante la Fase 1 (réplica) que no se arreglan ahora — se corrigen después,
en la fase de gobernanza. Cada entrada: qué es, dónde, por qué importa.

---

- [ ] 🔴 **`weekly_stats.ipynb` depende de un endpoint que se retrasa (`import_weekly_data()`),
  cuando podría calcular lo mismo desde `import_pbp_data()` (siempre al día).** Verificado en
  vivo en semana 3 de la temporada 2026: `import_schedules()`, `import_pbp_data()` y
  `import_depth_charts()` ya estaban al día (el *depth chart* con datos de ese mismo día), pero
  `import_weekly_data([2026])` regresó error 404 — el resumen semanal de `nflverse` todavía no
  se había publicado para la temporada en curso. Con el pipeline actual, esto bloquea correr
  cualquier semana mientras ese endpoint no se publique, aunque los datos crudos (PBP) para
  calcular las mismas stats ya existen. Gen1 (R) nunca tuvo este problema porque siempre agregó
  desde PBP directo, no desde un resumen. El pipeline nuevo en Python debe hacer lo mismo.

- [ ] 🔴 **Corrupción sistemática de códigos de equipo en el archivo de ADP de origen**, revisado
  en los 4 archivos finales (QB/RB/TE/WR, 278 jugadores): 7 casos reales de corrupción (2.5%),
  con un patrón específico — `HOU` (Houston) aparece truncado a `"HU"` en 3 jugadores distintos
  (Nick Chubb, Tank Dell, Christian Kirk) y `NO` (New Orleans) a `"N"` en 2 (Taysom Hill, Rashid
  Shaheed) — sugiere un find-replace o regex roto en el proceso que generó
  `data/adp_full_gsispos_*.csv`, no errores aislados. Además, a Mark Ingram (Jr.) le quedó
  `"II"` — el sufijo de su propio nombre — pegado en la columna `Team`, y a Donta Foreman
  `"RB79"` (posición+número, ya documentado antes). Aparte de la corrupción real, 12 filas más
  (4.3%) usan una convención de equipo distinta pero válida (`JAC` en vez de `JAX`, `LAR` en vez
  de `LA`) — no es un error, pero sí una inconsistencia que conviene normalizar.

- [x] **Notebook `weekly_stats.ipynb` duplicado** en dos rutas (`Weekly projections/WR/notebooks/`
  y `data/weekly_data/`), idéntico en lógica, solo cambia dónde guarda el CSV. La copia de
  `Weekly projections/WR/notebooks/` apunta a una carpeta ya marcada como "duplicado superado"
  en `.gitignore` (`Weekly projections/WR/data/`).
  **Resuelto (2026-09-28, reorg a `weekly/wr/`):** la revisión a fondo mostró que la acción
  sugerida original estaba al revés — `data/weekly_data/` era la copia **atrasada** (sin el fix
  de deduplicar `game_id`, solo semanas 7-8 corridas), y `Weekly projections/WR/` era la viva
  (54 archivos, semanas 7-18). Se conservó esa y se retiró `data/weekly_data/` por completo,
  junto con `weekly_stats.ipynb` mismo — confirmado que ni él ni la familia de notebooks que
  alimenta (`career_avg`, `last5_avg`, `season_avg`, `player_shares`) tienen consumidor real en
  el pipeline que sí corre (`wr_merge_stats.ipynb` solo ejecuta `wrs_rec_tds/yds/receptions`).

- [x] **Predicciones semanales duplicadas** en dos carpetas idénticas byte a byte:
  `Weekly projections/WR/outputs/2025/` y `data/weekly_data/weekly_predictions/2025/`
  (confirmado con `diff` en `wrs_complete_week7.csv` y `wrs_rec_yds_pred_week_7.csv`).
  **Resuelto (2026-09-28):** mismo cambio que el hallazgo anterior — `Weekly projections/WR/outputs/2025/`
  ahora vive en `weekly/wr/outputs/2025/`; `data/weekly_data/weekly_predictions/` se retiró.

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

- [ ] 🔴 **El marcador `"--"` de `gsis_id` (reservado para defensas de equipo) se filtra a
  jugadores reales sin cruce exitoso, en 4 de los 11 años del archivo de ADP.** En
  `data/adp_full_gsispos_*.csv`, `"--"` es válido para las 32 filas de defensa (`POS` empieza
  con `DST`) — pero de 2021 a 2024 también aparece en jugadores individuales cuyo cruce manual
  de `gsis_id` falló: 16 casos en 2021, 14 en 2022, 10 en 2023, 6 en 2024. No son siempre
  suplentes irrelevantes — ejemplo 2024: **DJ Chark Jr.**, receptor con una temporada de 1000+
  yardas en su carrera, cayó a ADP #284 por lesiones y quedó sin `gsis_id`, indistinguible de
  una defensa para cualquier cruce por id. En 2015-2020 y 2025 el cruce fue completo (0 casos)
  — el problema es específico de esos 4 años.

- [ ] 🔴 **La colisión de `gsis_id` ya documentada arriba (Brian Robinson Jr./Brock Purdy, y el
  triple de Colby Parkinson/Donta Foreman/Jeff Wilson Jr.) no se originó en `agg_ricky.ipynb` ni
  es exclusiva de 2024 — está directamente en `data/adp_full_gsispos_2024.csv`, y el mismo
  patrón aparece también en 2018 y 2019.** 2018: `00-0032135` compartido entre Jesse James y Joe
  Williams, y `00-0034419` entre Braxton Berrios y Richie James Jr. 2019: `00-0030108`
  duplicado para "Ryan Griffin" con y sin espacio final en el nombre — mismo jugador, dos filas.
  2024 suma un tercer caso además de los ya conocidos: `00-0035640` compartido entre Michael
  Pittman Jr. y DK Metcalf, dos titulares reales. Confirma que el archivo de origen nunca fue
  validado por unicidad de `gsis_id` en ningún año.

- [ ] **La profundidad del ranking de ADP no es consistente entre años — rompe cualquier
  comparación de tendencia.** `adp_full_gsispos_2019.csv` rankea hasta el jugador #1047 (1034
  filas), mientras el resto de los años se corta entre el #290 y el #488. Cualquier análisis que
  compare "percentil de ADP" o conteo de jugadores rankeados a través de temporadas estaría
  comparando un "top 1000" contra un "top 300-500" sin normalizar.

- [ ] 🔴 **`nflreadpy.load_depth_charts()` cambia de esquema entre 2024 y 2025 — sin columna
  compartida de temporada/semana entre ambos.** Verificado pidiendo cada año por separado:
  2022-2024 regresan `season`/`week`/`game_type`/`position`/`depth_team` (formato histórico,
  reconstruido por semana). 2025 en adelante — confirmado también para 2026, la temporada en
  curso — regresan un esquema completamente distinto: `dt`/`team`/`pos_grp`/`pos_rank`/`pos_abb`,
  **sin `season` ni `week` en absoluto**. Al pedir un rango que cruza el límite (`load_depth_charts([2024, 2025])`)
  la librería concatena ambos esquemas en una sola tabla: el resultado tiene ~94% de nulos en
  `position`/`depth_team`, que a simple vista parece un hueco de datos pero en realidad es que
  esas 554,215 filas (las de 2025) nunca tuvieron esas columnas — el dato real vive en `pos_abb`/
  `pos_grp`/`pos_rank` para esas filas. Cualquier código escrito contra el esquema viejo (como
  `wrs_rec_tds/yds/receptions.ipynb`, que usan `import_depth_charts()` de `nfl_data_py` para
  sacar el top-3 de WR por `pos_rank` de la semana en curso) truena o produce nulos silenciosos
  si se apunta directo a la temporada en curso sin un adaptador. Acción: el pipeline nuevo
  necesita una función que detecte el esquema por temporada (`season <= 2024` vs. `>= 2025`) y
  normalice ambos a las mismas columnas antes de usarlos juntos.

- [ ] 🔴 **Al construir el adaptador del hallazgo anterior, aparecieron 3 trampas más — ninguna
  truena, las 3 producen un resultado silenciosamente incorrecto si no se conocen:**
  1. **Esquema viejo — `position` sola no identifica el rol de ataque.** Un jugador puede tener
     `position="WR"` en una fila con `formation="Special Teams"` (ej. como regresador de
     despejes, `depth_position="PR"`), que no es su rol de receptor. Filtrar solo por `position`
     mezcla a ese jugador con los WR de ataque reales. Hace falta `formation=="Offense"` **y**
     `depth_position==<posición>` juntos.
  2. **Esquema nuevo — `pos_grp` no es la posición del jugador.** Son paquetes de formación
     (`"3WR 1TE"`, `"Base 4-3 D"`, `"Special Teams"`), no posiciones individuales. La posición
     real vive en `pos_abb`/`pos_name` (`"WR"` / `"Wide Receiver"`). Filtrar por `pos_grp=="WR"`
     no encuentra nada (ese valor no existe en la columna).
  3. **Esquema viejo — existe una "semana 19" etiquetada `game_type=="REG"`**, incluso en
     equipos que no llegaron a playoffs (confirmado con Atlanta 2024, que terminó 8-9 y no jugó
     postemporada). Parece un snapshot extra de fin de temporada mal etiquetado, no un partido
     real. Filtrar por `game_type=="REG"` no es suficiente por sí solo — hace falta acotar
     también `week <= 18`.

  Las dos funciones ya corregidas: `cargar_depth_charts_historico()` y
  `cargar_depth_charts_actual()` en `weekly/wr/src/data.py`.

- [ ] 🔴 **`cargar_depth_charts_historico()` podía regresar 2 filas para el mismo jugador en la
  misma semana, con `depth_team` distinto.** Confirmado en 357 de 27,928 combinaciones
  jugador-equipo-semana (1.3%) sobre 2016-2024 — ejemplo real: Braxton Miller, HOU, 2016 semana 1,
  aparece con `depth_team=2` y `depth_team=3` a la vez. El filtro ya existente
  (`formation=="Offense"` + `depth_position==<posición>` + `game_type=="REG"` + `week<=18`) no
  garantiza una sola fila por jugador-semana — el jugador queda listado más de una vez dentro de
  la misma formación/posición. Se descubrió al usar la función como insumo de un modelo en Fase 4:
  cualquier cruce por `(season, week, team, gsis_id)` duplicaba esas filas en silencio. Fix:
  `cargar_depth_charts_historico()` ahora se queda con el mejor rango (`depth_team` mínimo) por
  jugador-semana antes de regresar el resultado.
