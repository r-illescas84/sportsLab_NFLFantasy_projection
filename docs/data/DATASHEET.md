# Datasheet — datos usados en este proyecto

Basado en el formato de [Datasheets for Datasets](https://arxiv.org/abs/1803.09010). Documenta
de dónde salen los datos, qué tan buenos son, y qué alternativas se consideraron — antes de
construir features o modelos sobre ellos.

## Fuente

**`nflverse`** — ecosistema abierto de datos de la NFL, mantenido por la comunidad. Se puede leer
desde R (`nflfastR`+`nflreadr`) o Python. La verificación de que ambos leen la misma fuente
(Josh Allen 2023: 577 intentos/385 completos/4306 yardas/29 TDs, idéntico en R y Python) se hizo
con `nfl_data_py`, el wrapper de Python usado durante la Fase 1-3 de este proyecto — pero **el
pipeline nuevo usa `nflreadpy`** (ver "Riesgo de dependencia" abajo), su sucesor activo.

**Alternativas consideradas y por qué no se usan (todavía):**
- Sportradar / Stats Perform / SIS — datos de scouting propietarios (grades, tracking), de paga.
  Es lo que le da a PFF su ventaja de "calidad del jugador" más allá del volumen — no disponible
  para este proyecto por costo.
- APIs de casas de apuestas en vivo (líneas actualizadas al minuto) — `nflverse` ya trae líneas
  de cierre (`spread_line`/`total_line`) dentro de `import_schedules()`, sin costo ni API extra;
  cubre el caso de uso principal sin necesitar una fuente nueva.

**Decisión:** `nflverse` es la fuente única por ahora — es gratuita, abierta, y ya cubre línea de
Vegas y clima además de *play-by-play*. No se justifica pagar por una fuente de scouting
propietaria en esta etapa del proyecto.

## Riesgo de dependencia (verificado, no asumido)

- **`nflverse` no es una fuente propia — es un cliente que descarga datos ya armados** del
  repositorio `nflverse-data` en GitHub, actualizado por automatización (GitHub Actions). Si el
  proyecto dejara de mantenerse, no perdemos acceso de un día para otro, pero sí dejaríamos de
  recibir datos nuevos.
- **`nfl_data_py` (el paquete que se iba a usar) está oficialmente descontinuado** — su propio
  repositorio indica que no habrá más mantenimiento, y recomienda migrar a `nflreadpy` (el
  sucesor activo, mismo ecosistema). Decisión: el pipeline nuevo usa `nflreadpy`, no `nfl_data_py`
  (ver ADR `0002-fuente-de-datos.md`).
- **Las líneas de Vegas son la pieza más frágil de toda la fuente**: vienen del dataset de
  calendario mantenido por una sola persona de la comunidad (`nflverse/nfldata`, originalmente
  Lee Sharpe), como "líneas de cierre" de consenso de mercado — no se especifica de qué casa de
  apuestas. Es más frágil que el *play-by-play* (que sale de fuentes más directas/oficiales).
- **Mitigación decidida**: el pipeline nuevo guarda sus propias copias (*snapshots*) de lo que
  descarga, para no depender de que la fuente seguirá disponible hacia atrás en el tiempo. Si
  algún día esto se vuelve un bloqueo real, existen alternativas de pago (ej. The Odds API) para
  líneas de apuestas — pero sin historial gratuito hacia atrás como el que ya tiene `nflverse`.

## Auditoría por las 6 dimensiones de calidad de datos

| Dimensión | Qué se revisó | Resultado |
|---|---|---|
| **Exactitud** | Recalcular a mano stats de jugadores conocidos (Josh Allen, Tank Dell) contra el archivo publicado | Tank Dell aparece con el doble de sus stats reales (152 vs. 76 targets) — error real en el archivo de origen, no en los datos crudos de `nflverse` |
| **Completitud** | % de nulos en features candidatas nuevas (Fase 3); cruce de `gsis_id` en el archivo de ADP (11 años, 2015-2025) | `spread_line`/`total_line`: 100% (2016-2023). `temp`/`wind`: 65% (el resto son domos, no falta el dato, no aplica). `receiving_epa`/`target_share`/`wopr`: ~81% (el resto son jugadores sin target esa semana — no aplica, no es un hueco). `gsis_id`: completo en 2015-2020 y 2025, pero de 2021 a 2024 entre 6 y 16 jugadores reales por año quedan sin cruce y comparten el marcador reservado para defensas (`"--"`) |
| **Consistencia** | Nombres de columna repetidos, mismo dato en dos lados; profundidad del ranking de ADP entre años | `team_offense_season_avg_2014-24.csv` tiene 4 columnas duplicadas por nombre (mismo valor, dos veces). `adp_full_gsispos_2019.csv` rankea hasta el jugador #1047, mientras el resto de los años (2015-2025) se corta entre el #290 y el #488 — no comparable sin normalizar |
| **Validez** | Escaneo sistemático de `Team` en los 4 archivos finales (278 jugadores) | 7 casos de corrupción real (2.5%) con un patrón: `HOU`→`"HU"` (3 jugadores), `NO`→`"N"` (2), más 2 casos puntuales (sufijo de nombre pegado, posición+número pegado). 12 filas más (4.3%) usan una convención de equipo distinta pero válida (`JAC`/`LAR`) |
| **Vigencia** | ¿Los datos usados para una predicción se pueden auditar después? ¿Están disponibles a tiempo para la semana que viene? | Auditable desde la semana 5 de 2026: cada predicción guarda la tabla de variables con que se hizo (`variables_semana_N.csv`) y su huella en `tracking.csv`, y se evalúa sin recalcularla ([ADR 0006](../decisions/0006-seguimiento-semanal.md)). Antes, al evaluar una semana el flujo volvía a predecir y sobrescribía la predicción emitida. Verificado en vivo (semana 3 de 2026, con `nfl_data_py`): `import_schedules()`, `import_pbp_data()` y `import_depth_charts()` están al día, pero `import_weekly_data()` — de la que depende hoy `weekly_stats.ipynb` — **no está publicada todavía** para la temporada en curso (error 404). **Verificado de nuevo con `nflreadpy` el mismo día**: `load_player_stats(summary_level="week")` — su equivalente directo — **sí trae la temporada en curso al día** (hasta la última semana ya jugada, la 3). No hereda el rezago de `import_weekly_data` pese a ser el mismo tipo de resumen — se puede usar tanto para histórico como para la semana en curso, sin necesidad del cálculo manual desde PBP que se había planeado como *workaround* |
| **Unicidad** | `gsis_id` único por jugador en los 4 archivos finales, y en el archivo de ADP de origen (`data/adp_full_gsispos_*.csv`, 2015-2025) | 3 de 4 archivos finales (QB, TE, WR) tienen al menos un `gsis_id` compartido entre 2-3 jugadores distintos de posiciones diferentes. Rastreado hasta el origen: la misma colisión ya está en `adp_full_gsispos_2024.csv`, y el mismo patrón aparece también en 2018 y 2019 — nunca se validó unicidad de `gsis_id` en ningún año del archivo fuente |

Detalle técnico completo de cada hallazgo en [`../HALLAZGOS.md`](../HALLAZGOS.md).

### En cada corrida del flujo semanal (Fase 6)

Esta auditoría se hizo una vez. Desde la Fase 6, `weekly/wr/src/seguimiento.py` revisa las seis
dimensiones en cada corrida, sobre la tabla de la semana: unicidad (una fila por jugador),
completitud (variables del modelo presentes y vacíos contra el histórico de la misma semana),
consistencia (cada equipo que juega tiene alineación), validez (valores dentro del rango histórico),
exactitud (en semanas jugadas, `fantasy_points_ppr` = `fantasy_points` + recepciones y recepciones ≤
pases dirigidos) y vigencia (estadísticas de la semana anterior cargadas, líneas de apuestas en cada
partido). Lo que invalida la predicción detiene la corrida; lo demás queda como advertencia en
`weekly/wr/outputs/tracking.csv`. La tabla completa de chequeos está en el
[ADR 0006](../decisions/0006-seguimiento-semanal.md).

## Auditoría de `nflreadpy` por fuente (Fase 2)

Antes de construir el pipeline nuevo sobre `nflreadpy`, se auditó cada una de las 5 funciones que
usará — no solo unas semanas de la temporada en curso, sino **2 temporadas completas y cerradas
(2024-2025)**, para no dejar pasar algo que solo aparece con volumen real. Script parametrizable
por rango de temporadas (no atado a estos 2 años).

| Fuente | Qué se encontró | Acción |
|---|---|---|
| `load_players()` | 24,834 jugadores históricos, `gsis_id` 0% nulo, **0 colisiones** — a diferencia del archivo de ADP, que sí las tenía | Confirma la decisión ya tomada: identidad desde aquí, no desde el ADP. `draft_year`/`draft_round` 49.7% nulo es estructural (jugadores no drafteados) |
| `load_schedules([2024,2025])` | 570 partidos, no los 544 esperados — incluye playoffs (semanas 19-22, `game_type` WC/DIV/CON/SB/SBBYE). `spread_line`/`total_line` 0% nulo. `temp`/`wind` 34.7% nulo (domos) | Filtrar explícitamente `game_type == "REG"` en el pipeline — si no, se mezclan playoffs sin querer |
| `load_player_stats([2024,2025], summary_level="week")` | 38,405 filas, 5,195 solo WR, **0 combinaciones (jugador, temporada, semana) duplicadas**. Columnas núcleo (`receptions`, `receiving_yards`, `target_share`, etc.) 0% nulo. `racr`/`receiving_epa` ~15% nulo — verificado: 796/801 (99.4%) son semanas con 0 targets (estructural); 5 filas sin explicación clara, volumen mínimo. **También mezcla playoffs** (columna `season_type`: `REG`/`POST`, semanas hasta la 22) | Filtrar `season_type == "REG"`, mismo patrón que `load_schedules`. Las 5 filas sin explicar se documentan, no bloquean — se revisan en Fase 4 si esas columnas terminan usándose |
| `load_pbp([2024,2025])` | 98,263 jugadas. `receiver_id`/`air_yards` ~62% nulo (estructural — solo pases tienen receptor). **También mezcla playoffs** (`season_type`) | Filtrar `season_type == "REG"`, mismo patrón |
| `load_depth_charts([2024,2025])` | 🔴 **Dos esquemas incompatibles concatenados.** 2022-2024 traen `season`/`week`/`game_type`/`position`/`depth_team`. **2025 en adelante (confirmado también para 2026, la temporada en curso) cambia a un esquema sin `season` ni `week`** (`dt`/`pos_grp`/`pos_rank`/`pos_abb`). Pedir `[2024, 2025]` junto concatena ambos esquemas y produce ~94% de "nulos" que en realidad es "esa columna no existe para ese año" — no falta el dato, el dato vive en otra columna. Además, ninguno de los dos esquemas se puede filtrar solo por el nombre obvio de columna (`position`/`pos_grp`) sin producir un resultado silenciosamente incorrecto — ver `HALLAZGOS.md` | Resuelto con 2 funciones separadas en `weekly/wr/src/data.py` (`cargar_depth_charts_historico` / `cargar_depth_charts_actual`), cada una con el filtro correcto verificado contra datos reales. Después se unificaron en una sola tabla con semana de partido para los dos esquemas (`data.cargar_depth_charts_unificado`, ver `weekly/wr/notebooks/1.3_unificacion_alineaciones.ipynb`): `depth_team` pasó de 0% a ~97% de cobertura en 2025 |

### Verificación de tipos de dato (Polars → pandas)

No basta con revisar nulos y duplicados — un valor puede "verse bien" y aun así venir en un tipo
de dato que no tiene sentido para lo que representa (una fecha guardada como texto, por ejemplo).
Se revisó explícitamente, columna por columna, en las 4 fuentes ya integradas a `data.py`:

| Columna | Lo que se encontró | ¿Tiene sentido? | Acción |
|---|---|---|---|
| `birth_date` (`load_players`), `gameday` (`load_schedules`) | Llegan como texto, aunque el contenido tiene forma de fecha ISO (`'1987-01-25'`) | No — un texto no permite calcular edad ni comparar fechas directamente | Convertidas explícitamente a fecha real (`pd.to_datetime`) dentro de `data.py` |
| `draft_year`, `draft_round`, `height`, `weight` (`load_players`); `temp`, `wind` (`load_schedules`) | Enteros en Polars, se vuelven `float64` al pasar a pandas (ej. `2023.0` en vez de `2023`) | Sí — pandas no tiene un entero clásico que admita nulos, y estas columnas sí tienen nulos; sube a `float64` automáticamente. No es un error de la fuente | Ninguna — funciona igual para un modelo. Si se necesitara mostrarlos como enteros limpios, usar el tipo nulable `Int64` de pandas |
| `receptions`, `receiving_yards`, `receiving_tds`, `targets` (`load_player_stats`) | Se mantienen como enteros en ambos formatos | Sí — no tienen nulos, por eso no hay upcast a `float64` | Ninguna |
| `pass_attempt`, `complete_pass` (`load_pbp`) | `float64` (0.0/1.0), no entero ni booleano, incluso ya en Polars | Sí — verificado que sí tienen nulos (3.1% de las jugadas, ej. tiempos fuera) donde "fue intento de pase" no aplica; `float` es la única forma de representar 0/1/no-aplica sin un booleano nulable | Ninguna |

## Qué significa esto para el proyecto

- Los datos crudos de `nflverse` (*play-by-play*, semanales, calendario) son confiables — los
  problemas de exactitud/validez/unicidad encontrados están en el archivo de **ADP de origen**
  (`data/adp_full_gsispos_*.csv` y sus derivados), no en la fuente de datos en sí.
- El pipeline nuevo (`pipeline/players.py`, según el plan) debe reconstruir el mapeo
  jugador→`gsis_id` directamente desde `nflreadpy.load_players()`/`load_rosters()`, no heredar el
  archivo de ADP con estos errores ya confirmados.
- Los "nulos" en las features nuevas candidatas (clima, EPA, target share) son mayoritariamente
  estructurales (no aplica), no datos faltantes reales — no requieren imputación agresiva, sí un
  indicador explícito de "no aplica" (ej. domo, sin targets esa semana).
- **Corregido tras verificar con `nflreadpy` (Fase 2):** se había planeado calcular stats
  semanales desde `load_pbp()` para evitar el rezago de `import_weekly_data()` — verificado en
  vivo que `load_player_stats(summary_level="week")` **no tiene ese rezago**, así que el
  pipeline nuevo la usa directamente, tanto para histórico como para la semana en curso. Se
  mantiene `load_pbp()` como fuente aparte solo para lo que `load_player_stats()` no cubre
  (`target_share` y agregados propios).
- `nflreadpy` regresa `Polars` DataFrames por defecto (no `pandas`) — usar `.to_pandas()` al
  integrar con el resto del pipeline, que sigue en `pandas`. Requiere `pyarrow` instalado (no es
  dependencia automática de `nflreadpy` ni de `pandas`); ya agregado a `docker/requirements.txt`.
