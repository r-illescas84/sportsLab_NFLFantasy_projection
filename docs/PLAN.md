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
| 3. EDA de WR | Explicar cada target con un ejemplo real antes de graficar; distribución de los 3 targets; decidir `last3` vs `last5` con evidencia | Notebook de EDA + decisión documentada de la ventana | Pendiente |
| 4. Feature engineering informado | Auditar qué ya está disponible y sin usar (EPA, `air_yards`, `wopr`); no forzar Vegas/clima | Lista de features con su justificación, en `features.py` | Pendiente |
| 5. Modelado comparado | Patrón de `03_modelo_predictivo` (Baseline/Ridge/RandomForest/XGBoost, split temporal) + cuantiles | `modeling.py` parametrizado por `target` + Model Card por target | Pendiente |
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
