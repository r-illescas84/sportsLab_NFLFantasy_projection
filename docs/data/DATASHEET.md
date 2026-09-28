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
  jugador→`gsis_id` directamente desde `nflreadpy.load_players()`/`load_rosters()`, no heredar el
  archivo de ADP con estos errores ya confirmados.
- Los "nulos" en las features nuevas candidatas (clima, EPA, target share) son mayoritariamente
  estructurales (no aplica), no datos faltantes reales — no requieren imputación agresiva, sí un
  indicador explícito de "no aplica" (ej. domo, sin targets esa semana).
- **El pipeline nuevo debe calcular stats semanales desde `load_pbp()`, no depender de
  `load_player_stats()`** — el PBP está al día (verificado en semana 3 de 2026 con el equivalente
  en `nfl_data_py`), el resumen semanal tiene rezago y puede no estar publicado cuando se
  necesita correr la semana en curso. Es literalmente lo que ya hace Gen1 (R): agrega desde PBP
  directo, nunca de un resumen.
- `nflreadpy` regresa `Polars` DataFrames por defecto (no `pandas`) — usar `.to_pandas()` al
  integrar con el resto del pipeline, que sigue en `pandas`.
