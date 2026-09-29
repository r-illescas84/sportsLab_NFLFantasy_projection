# sportsLab_NFLFantasy_projection

Proyecciones de jugadores de NFL para fantasy football.

## Estructura

- **`weekly/`** — línea de trabajo activa: proyecciones semana a semana. Empieza con `wr/`
  (receptores), de principio a fin (datos → EDA → features → modelo comparado → tracking) — es
  la guía que se replica para las demás posiciones.
- **`annual/`** — proyecciones a nivel temporada completa, en pausa (ver
  `docs/decisions/0003-alcance-wr-semanal.md`). No descartado, solo fuera de alcance mientras la
  temporada ya está en curso.
- **`data/`** — datos compartidos entre ambas líneas de trabajo (ADP, archivos crudos).
- **`docs/`** — `BITACORA.md` (continuidad entre sesiones), `decisions/` (por qué se decidió
  cada cosa), `data/DATASHEET.md` (de dónde salen los datos y qué tan buenos son), y
  `HALLAZGOS.md` (problemas encontrados, pendientes de resolver).
- **`docker/`** — entorno reproducible (Python + R).

Cada carpeta de trabajo trae su propio `README.md` con el detalle de lo que contiene.
