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

- [x] 🔴 **`nflreadpy.load_depth_charts()` cambia de esquema entre 2024 y 2025 — sin columna
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
  **Resuelto (2026-09-30, `1.3_unificacion_alineaciones.ipynb`):** `data.cargar_depth_charts_unificado()`
  mapea cada `dt` (esquema 2025+) al próximo partido real de ese equipo (`pd.merge_asof`,
  `direction="forward"`, contra el calendario) y se queda con el snapshot más cercano al partido
  por equipo-semana. Verificado con 2 casos reales (Marvin Harrison Jr., Ja'Marr Chase, ambos WR1
  casi toda la temporada 2025) y sin filas duplicadas jugador-equipo-semana. Cobertura de
  `depth_team` sobre filas WR reales de 2025 sube de 0% a ~97%. Integrada en
  `features.construir_tabla_modelado` — ya no es una limitación de los modelos guardados.

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
     también la semana a la última real de temporada regular (17 hasta 2020, 18 desde 2021; ver
     el hallazgo de códigos de equipo, más abajo).

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

- [ ] 🔴 **Los CSVs de predicciones del pipeline heredado (`weekly/wr/outputs/2025/*_pred_week_N.csv`
  y `wrs_complete_weekN.csv`) tienen filas duplicadas por jugador-semana, con predicciones
  distintas entre sí.** Confirmado semana por semana (7 a 18 de 2025): semanas 7, 8 y 10 sin
  duplicar; semana 9 duplica solo `receiving_tds` (1.89x); semana 11 y 13-17 duplican levemente
  (1.02x-1.06x); semana 12 duplica exactamente al doble (2.00x); semana 18 duplica 1.85x en los 3
  targets a la vez (`wrs_complete_week18.csv`: 730 filas para 96 jugadores únicos, 7.6x). No es un
  patrón uniforme — parece que algunas corridas del pipeline heredado guardaron más de un
  *snapshot* de depth chart del mismo día para la misma semana, sin deduplicar antes de aplicar el
  modelo. La dispersión entre duplicados del mismo jugador-semana es real, no cosmética: mediana
  ~27% de diferencia relativa entre las filas duplicadas, hasta 2-3 veces esa magnitud en casos
  extremos (ejemplo real: Adonai Mitchell, semana 18, `predicted_receiving_tds` = 0.345 / 0.250 /
  0.149 en sus 3 filas). Se descubrió al construir el benchmark de Fase 5
  (`weekly/wr/notebooks/5.2_benchmark_vs_legado.ipynb`) contra las predicciones reales ya guardadas
  — tomar "la primera fila" a ciegas habría introducido un error arbitrario en la comparación. Se
  resolvió ahí con una regla explícita (promediar los duplicados por jugador-semana antes de
  calcular cualquier métrica), pero el defecto de origen sigue sin corregirse en el pipeline
  heredado — queda fuera de alcance de este proyecto, documentado aquí para quien lo retome.

- [x] 🔴 **`XGBRegressor().load_model()` falla al recargar un modelo guardado, con las versiones
  fijadas del proyecto** — `AttributeError: 'super' object has no attribute '__sklearn_tags__'`
  (xgboost 2.1.3 con scikit-learn 1.6.1), encontrado al verificar la recarga de los modelos
  guardados en `6.1_modelo_final_y_temporada_actual.ipynb`. Guardar con `save_model()` funciona; lo que
  falla es recargar con el wrapper de sklearn. **Resuelto:** se recarga con `xgboost.Booster()` y
  `DMatrix`, que es el camino que usa `modeling.cargar_modelos()`.

- [x] 🔴 **`idata.to_netcdf()` de `arviz` guarda sin error pero falla al recargar** —
  incompatibilidad real entre las versiones instaladas de `h5netcdf`/`xarray`
  (`AttributeError: 'Variable' object has no attribute 'filters'`, confirmado en
  `4.7_jerarquico_touchdowns.ipynb`). Mismo tipo de problema que el bug de
  `XGBRegressor().load_model()` con el wrapper de sklearn ya documentado arriba — una
  incompatibilidad entre librerías de terceros en versiones recientes, no un error del código del
  proyecto. **Resuelto:** se usa `joblib` para guardar/recargar el objeto `idata` completo (mismo
  criterio ya usado para los modelos de cuantil/ensamble) — verificado con una recarga real que
  reproduce las mismas métricas de test.

- [x] **`receiving_yards` tiene valores negativos reales — rompe una transformación `log1p`
  directa.** 67 filas en el set de entrenamiento 2016-2021 (confirmado en
  `4.8_jerarquico_recepciones_yardas.ipynb`), ej. Eddie Royal, 2016 semana 12, 1 recepción con -6
  yardas netas. No es un error de datos: son jugadas reales donde el receptor fue tackleado
  detrás de la línea de golpeo (ej. un *screen pass* o motion en jet sweep que termina en
  pérdida) — un jugador puede terminar el partido con yardas de recepción netas negativas.
  `np.log1p()` no está definido para valores menores a -1 (varias de estas filas llegan hasta
  -9), así que una transformación logarítmica directa sobre `receiving_yards` falla o produce
  `NaN`/`-inf` en silencio si no se revisa antes. **Resuelto:** se usa una transformación
  logarítmica con signo (`sign(x) * log1p(|x|)`), que maneja negativos correctamente y mapea
  0 → 0 exacto.

- [x] **`cargar_depth_charts_unificado()` asigna el scrape de la mañana de un partido al partido
  siguiente — correr el flujo antes de que cierre la semana anterior da una alineación parcial.**
  Un `dt` se asigna al primer partido con `gameday >= dt`, y `gameday` es medianoche: un scrape de
  las 07:15 UTC del día del partido queda después y cae en el siguiente. Confirmado el 2026-10-01
  (jueves de la semana 4): la semana 5 tenía alineación de solo 2 de 30 equipos (CLE y PIT, que
  juegan ese jueves), y el flujo devolvía 12 WR sin avisar. Para la semana que sí está completa no
  afecta: usa el último scrape anterior al partido. **Resuelto:** `pipeline.ejecutar_semana` compara
  los equipos con alineación contra los del calendario y falla con un mensaje que dice cuáles
  faltan.

- [x] 🔴 **La selección de modelos usaba MAE, que premia la mediana, cuando los modelos predicen
  valores esperados.** En touchdowns (82% de ceros) la mediana es 0: en validación 2022-2023,
  predecir 0 para todos tiene MAE de 0.193 contra 0.336 de predecir la media de entrenamiento, pero
  su D² es −6.13 (`4.1_preparacion_y_metricas.ipynb`). Elegir por MAE favorece modelos que predicen
  casi cero. **Resuelto:** política de métricas por resultado (RMSE en recepciones y yardas, deviance
  de Poisson en touchdowns) con requisitos de calibración —
  [`decisions/0004-metricas-de-seleccion.md`](decisions/0004-metricas-de-seleccion.md).

- [x] 🔴 **Varias decisiones de modelado se tomaron mirando el conjunto de prueba.** El ganador de
  touchdowns se eligió por MAE de prueba (`tabla_final.sort_values("test_mae")`); la adopción de
  hiperparámetros se decidió con un umbral de mejora en MAE de prueba; y la comparación «entrenar con
  todas las semanas o solo con las activas» se evaluó en prueba. La prueba solo debe reportarse una
  vez. **Resuelto:** las tres decisiones se toman en validación (`4.2`, `4.3` y `4.4`).

- [x] 🔴 **El modelo de touchdowns usaba los hiperparámetros por defecto de XGBoost (tasa 0.3,
  profundidad 6, 100 árboles) y quedó descalibrado.** En validación: pendiente de calibración 0.57
  (predicciones demasiado extremas), sesgo −0.075 y D² −0.206; en prueba, R² −0.040. La
  descomposición de Gneiting y Resin (2023) muestra que su descalibración (0.0134) es mayor que su
  discriminación (0.0112), y de ahí la R² negativa (`4.3_modelos_touchdowns.ipynb`).
  **Resuelto:** sus hiperparámetros se eligen ahora con la grilla de la familia y deviance de Poisson
  en validación.

- [x] **Dos fugas pequeñas en `experimentos.entrenar_y_evaluar`.** El respaldo del baseline (la
  media cuando el jugador no tiene promedios) se calculaba con todas las temporadas, prueba incluida,
  y el imputador de RandomForest se ajustaba sobre todas las filas. **Resuelto:** ambos se ajustan
  solo con entrenamiento.

- [x] **El primer requisito de sesgo («sesgo total de a lo más 5% de la media») estaba mal
  planteado.** La tasa de touchdowns por receptor bajó de 0.216 (2016-2021) a 0.193 (2022-2023): hasta
  predecir la media de entrenamiento tiene 12% de sesgo en validación, así que ningún modelo
  entrenado con años anteriores podía cumplirlo. **Resuelto:** el requisito mide el sesgo que el
  modelo agrega por encima del de la media de entrenamiento (`experimentos.REQUISITOS`).

- [x] **Enlaces relativos rotos a `docs/` en `1.1_calidad_fuentes.ipynb`.** Usaban `../../docs/`
  cuando desde `weekly/wr/notebooks/` la ruta correcta es `../../../docs/`. **Resuelto.**

- [x] **La selección de variables usó como validación los mismos años que después son prueba del
  modelado.** `3.2_seleccion_features.ipynb` eligió variables con validación 2024-2025, que en la
  etapa 4 es el conjunto de prueba: el resultado de prueba podía ser algo optimista. Además, su
  importancia por permutación se midió con MAE, que en touchdowns deja casi todas las importancias en
  cero, y el corte por «las 15 más importantes de cada resultado» era inestable: con el dato de
  alineaciones corregido, la misma regla daba 31 variables en lugar de 34. **Resuelto** en
  `3.3_seleccion_estable.ipynb` (ADR 0005): orden estable por eliminación recursiva con la métrica
  principal, sin tocar la validación, y tamaño elegido en validación 2022-2023 con una prueba de no
  inferioridad. Quedan 25 variables.

- [x] **La primera regla para elegir el tamaño del conjunto confundía empate con no inferioridad.**
  Elegía el conjunto más chico cuyo intervalo de diferencia contra el mejor incluyera cero, con el nivel
  ajustado por Bonferroni. Así gana el conjunto cuyas diferencias tienen más ruido, no el que pierde
  menos: elegía 10 variables mientras que los conjuntos de 15 a 40 quedaban fuera, y el ajuste, al
  ensanchar los intervalos, lo agrava. Se hizo visible al reportar la prueba, donde ese conjunto quedaba
  peor que las 34 anteriores en recepciones. **Resuelto:** prueba de no inferioridad con margen de 1%
  (la cota superior del intervalo de 95% no debe pasar de 1% de la pérdida del mejor), aplicada solo en
  validación (`experimentos.elegir_conjunto_no_inferior`). La prueba ya se había visto al corregir; la
  verificación limpia es la temporada 2026.

- [x] **4.4 y 4.6 no aplicaban el ajuste por comparaciones múltiples del ADR 0004.** Comparaban la
  mejor de varias alternativas contra el modelo elegido con intervalos de 95%. Con las 25 variables, el
  ajuste compacto de recepciones (4.4) y los dos ensambles de recepciones (4.6) excluían el cero así.
  **Resuelto:** 4.4 usa el nivel ajustado por las 30 combinaciones y 4.6 por sus 4 comparaciones. Con
  él, el ajuste compacto empata; el ensamble de cuatro modelos sigue mejorando (0.26% del RMSE) y no se
  adopta por su costo (ADR 0004: la regla es condición necesaria, no suficiente).

- [x] **El modelo jerárquico de yardas se comparaba sin corregir la retransformación.** Se ajusta en
  escala logarítmica, y deshacer el logaritmo del promedio da algo cercano a la mediana, no al valor
  esperado: en validación subestima 10.8 yardas por receptor, y aun así tiene el MAE más bajo de su
  tabla. La corrección estándar de Duan (1983) se va al extremo contrario (+12.5 yardas, pendiente
  0.44). **Resuelto en la comparación:** `4.8_jerarquico_recepciones_yardas.ipynb` reporta las dos
  versiones; ninguna se adopta.

- [x] **El «techo» de R² de touchdowns de `4.1_preparacion_y_metricas.ipynb` suponía Poisson.** El
  cálculo (varianza − media) / varianza da 0.06–0.07, pero el modelo elegido llega a 0.08–0.09
  fuera de muestra: casi nunca hay más de un touchdown por partido, así que el azar real es menor que
  el de una Poisson. **Resuelto:** 4.1 lo presenta como referencia bajo ese supuesto, no como tope.

- [x] **La regla de empates no controlaba comparaciones múltiples.** Un intervalo de 95% por
  comparación deja que, entre muchas alternativas, alguna parezca mejor por azar. En
  `4.9_ventana_entrenamiento.ipynb`, 1 de 15 alternativas (ponderación por antigüedad en recepciones)
  excluía el cero con 95% y no con el intervalo ajustado (con el dato de alineaciones anterior a su
  corrección; con el corregido ya empata con 95%). Con el dato corregido, el caso que el ajuste sí
  cambia es la mejora en recepciones contra el flujo heredado en `5.2_benchmark_vs_legado.ipynb`.
  **Resuelto:** el ADR 0004 y `4.1_preparacion_y_metricas.ipynb` piden ajustar por Bonferroni cuando se
  comparan varias alternativas; `experimentos.diferencia_bootstrap` acepta `nivel`.

- [x] **Las alineaciones históricas usaban otros códigos de equipo que las estadísticas.**
  `load_depth_charts` usa el código de la época (`OAK` hasta 2019, `SD` en 2016) y
  `load_player_stats` el de la franquicia actual (`LV`, `LAC`) en todas las temporadas. El cruce por
  equipo dejaba sin `depth_team` a todos los receptores de los Raiders 2016-2019 y de los Chargers
  2016: 315 filas de WR que sí tenían alineación. Además, las temporadas 2016-2020 (17 semanas)
  traían una «semana 18» de temporada regular, el mismo error de etiqueta que la «semana 19» de años
  recientes. **Resuelto** en `data.cargar_depth_charts_historico`: los códigos se traducen al actual
  y la semana se acota a 17 hasta 2020 y a 18 desde 2021. La cobertura de `depth_team` de 2016-2019
  sube de 90.5-92.2% a 93.4-96.3%. Con los modelos elegidos, la métrica principal de validación
  cambia a lo más 0.11% (yardas 29.538 → 29.572; recepciones y touchdowns, empate) y todos siguen
  cumpliendo los requisitos.

- [x] **El total implícito de las líneas de apuestas se calculaba con la línea invertida.** En
  `nflreadpy`, `spread_line` positivo significa que el local es favorito (correlaciona +0.50 con el
  margen real del local en 2024-2025), pero `2.3_eda_general.ipynb` calculaba el total del local como
  `(total − spread)/2`. Con esa fórmula el total implícito correlacionaba −0.19 con los puntos reales
  del local; corregido, +0.43. Su correlación con las yardas de un WR pasa de 0.0018 a 0.088.
  **Resuelto** en 2.3. De ese resultado salía la conclusión «Vegas/clima sin efecto», por la que el
  total implícito nunca entró a la evaluación de `3.2_seleccion_features.ipynb`.

- [x] **Evaluar el total implícito como variable.** Queda como candidata para la selección de
  variables que se rehará (ver la entrada de `3.2_seleccion_features.ipynb` arriba). **Resuelto** en
  `3.3_seleccion_estable.ipynb`: queda octavo en el orden estable y entra entre las 25 variables.

- [x] **El flujo semanal sobrescribía la predicción emitida al evaluar la semana.** Cuando la semana
  ya se había jugado, `pipeline.ejecutar_semana` volvía a predecir con los modelos de ese momento y
  escribía encima de `predicciones_semana_N.csv`. Lo que se predijo antes de los partidos se perdía, y
  si los modelos cambiaban entre una corrida y otra, la evaluación medía a un modelo que no había
  hecho esa predicción. **Resuelto** (ADR 0006): la predicción emitida y sus variables se guardan
  aparte y la evaluación las lee sin modificarlas (`evaluacion_semana_N.csv`). Las semanas 1-4 de
  2026 quedan en el historial como `reconstruida`.

- [x] **La primera regla de alertas habría sonado todas las temporadas.** Con límites en los
  percentiles 2.5 y 97.5 de cada serie y «persistente» como dos ventanas seguidas fuera, las cinco
  temporadas 2021-2025 habrían tenido alguna alerta persistente, aunque el modelo cumplió los
  requisitos en todas. La prueba calcula los límites de cada temporada sin ella. Hay dos causas: se
  vigilan nueve series a la vez (tres resultados por tres métricas), y dos ventanas seguidas
  comparten tres de sus cuatro semanas. **Resuelto** antes de adoptarla, en
  `6.3_seguimiento_semanal.ipynb`: el 5% se reparte entre las nueve series (percentiles 0.28 y
  99.72) y la persistencia se mide contra la ventana sin semanas en común. Con eso queda una
  temporada de cinco con alerta persistente.

- [x] **`3.3_seleccion_estable.ipynb` decía usar la configuración de 4.4, pero en touchdowns usaba la
  anterior.** La selección se hizo con la configuración vigente en ese momento, elegida con las 34
  variables: 200 árboles con tasa 0.08. Con las 25 variables, 4.3 llega después a 400 árboles con tasa
  0.03, y el texto seguía remitiendo a 4.4. **Resuelto:** el texto dice qué configuración se usó, y la
  sección 7 de 3.3 repite la selección con la final. Da el mismo tamaño (25) y cambia una variable en
  el borde del orden: entra `targets_last3_avg` y sale `air_yards_share_last3_avg`. Las 25 adoptadas
  también pasan la regla, con pérdidas de validación prácticamente iguales, y se mantienen.
