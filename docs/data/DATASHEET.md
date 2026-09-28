# Datasheet — datos usados en este proyecto

Basado en el formato de [Datasheets for Datasets](https://arxiv.org/abs/1803.09010). Documenta
de dónde salen los datos, qué tan buenos son, y qué alternativas se consideraron — antes de
construir features o modelos sobre ellos.

## Fuente

**`nflverse`** (vía `nfl_data_py` en Python / `nflfastR`+`nflreadr` en R) — ecosistema abierto de
datos de la NFL, mantenido por la comunidad. Confirmado en esta fase que ambos wrappers leen
exactamente la misma fuente (verificado con Josh Allen 2023: 577 intentos/385 completos/4306
yardas/29 TDs, idéntico en ambos).

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

## Auditoría por las 6 dimensiones de calidad de datos

| Dimensión | Qué se revisó | Resultado |
|---|---|---|
| **Exactitud** | Recalcular a mano stats de jugadores conocidos (Josh Allen, Tank Dell) contra el archivo publicado | Tank Dell aparece con el doble de sus stats reales (152 vs. 76 targets) — error real en el archivo de origen, no en los datos crudos de `nflverse` |
| **Completitud** | % de nulos en features candidatas nuevas (Fase 3) | `spread_line`/`total_line`: 100% (2016-2023). `temp`/`wind`: 65% (el resto son domos, no falta el dato, no aplica). `receiving_epa`/`target_share`/`wopr`: ~81% (el resto son jugadores sin target esa semana — no aplica, no es un hueco) |
| **Consistencia** | Nombres de columna repetidos, mismo dato en dos lados | `team_offense_season_avg_2014-24.csv` tiene 4 columnas duplicadas por nombre (mismo valor, dos veces) |
| **Validez** | Escaneo sistemático de `Team` en los 4 archivos finales (278 jugadores) | 7 casos de corrupción real (2.5%) con un patrón: `HOU`→`"HU"` (3 jugadores), `NO`→`"N"` (2), más 2 casos puntuales (sufijo de nombre pegado, posición+número pegado). 12 filas más (4.3%) usan una convención de equipo distinta pero válida (`JAC`/`LAR`) |
| **Vigencia** | ¿Los datos usados para una predicción se pueden auditar después? ¿Están disponibles a tiempo para la semana que viene? | No auditable hoy — depende de un *snapshot* de "hoy" que no se guarda (ver Fase 5). Además, verificado en vivo (semana 3 de 2026): `import_schedules()`, `import_pbp_data()` y `import_depth_charts()` están al día, pero `import_weekly_data()` — de la que depende hoy `weekly_stats.ipynb` — **no está publicada todavía** para la temporada en curso (error 404). El pipeline actual no puede correr para la semana en curso por esta dependencia, aunque los datos crudos (PBP) sí existen |
| **Unicidad** | `gsis_id` único por jugador en los 4 archivos | 3 de 4 archivos (QB, TE, WR) tienen al menos un `gsis_id` compartido entre 2-3 jugadores distintos de posiciones diferentes |

Detalle técnico completo de cada hallazgo en [`../HALLAZGOS.md`](../HALLAZGOS.md).

## Qué significa esto para el proyecto

- Los datos crudos de `nflverse` (*play-by-play*, semanales, calendario) son confiables — los
  problemas de exactitud/validez/unicidad encontrados están en el archivo de **ADP de origen**
  (`data/adp_full_gsispos_*.csv` y sus derivados), no en la fuente de datos en sí.
- El pipeline nuevo (`pipeline/players.py`, según el plan) debe reconstruir el mapeo
  jugador→`gsis_id` directamente desde `nfl_data_py.import_players()`/rosters, no heredar el
  archivo de ADP con estos errores ya confirmados.
- Los "nulos" en las features nuevas candidatas (clima, EPA, target share) son mayoritariamente
  estructurales (no aplica), no datos faltantes reales — no requieren imputación agresiva, sí un
  indicador explícito de "no aplica" (ej. domo, sin targets esa semana).
- **El pipeline nuevo debe calcular stats semanales desde `import_pbp_data()`, no depender de
  `import_weekly_data()`** — el PBP está al día (verificado en semana 3 de 2026), el resumen
  semanal tiene rezago y puede no estar publicado cuando se necesita correr la semana en curso.
  Es literalmente lo que ya hace Gen1 (R): agrega desde PBP directo, nunca de un resumen.
