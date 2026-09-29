# 0003 — Alcance: WR semanal primero, anual en pausa

**Fecha:** 2026-09-28
**Estado:** aceptada

## Contexto

El equipo se reunió y revisó el avance del proyecto. Dos observaciones cambiaron el alcance:

1. La parte anual (Gen1, R, nivel temporada) está pensada para correr **antes** de que arranque
   la temporada de NFL, usando la temporada anterior como referencia. La temporada actual ya
   empezó — seguir desarrollándola ahora no tiene un uso real hasta la próxima entresemana.
2. Ricky se hizo cargo del desarrollo de las demás posiciones (QB/RB/TE) y entregará después un
   notebook propio. El trabajo de WR, hecho primero y a fondo, es la guía que se va a adaptar
   para esas posiciones — no un desarrollo aislado.

## Opciones consideradas

1. **Seguir avanzando las 4 posiciones en paralelo** — reparte el esfuerzo, pero nadie termina
   un ciclo completo (datos → EDA → modelo → tracking) a tiempo para que sirva de referencia.
2. **Pausar lo anual, enfocar todo en WR semanal, documentar el patrón para replicar después**
   — un ciclo completo termina antes, y sirve de plantilla real cuando lleguen las demás
   posiciones.

## Decisión

Opción 2. La parte anual (carpetas `QB/`, `RB/`, `TE/`, `WR/`, `DST/`, `K/`, `Rookies/`,
`Models/`, `extras/`, `renv/` en la raíz) se mueve a `annual/` — en pausa, no descartada. El
trabajo activo es `weekly/wr/`: ciclo completo (datos, EDA, feature engineering, modelado
comparado, estabilidad) sobre la posición WR a nivel semanal.

## Consecuencias

- `annual/` no se toca hasta que el equipo decida retomarlo (probablemente antes de la próxima
  temporada). Para correr algo ahí, hace falta `cd annual` primero — `renv`/`.Rprofile` viven
  juntos en esa carpeta.
- El pipeline de `weekly/wr/` se diseña parametrizado (target, semana, temporada) desde el
  principio, pensando en que se reutilizará para las demás posiciones cuando llegue el notebook
  de Ricky — sin generalizar el código todavía para posiciones que no existen en el repo hoy.
- Resuelve, de paso, 2 hallazgos de duplicidad ya documentados en `docs/HALLAZGOS.md`
  (`Weekly projections/WR/` vs. `data/weekly_data/` eran el mismo pipeline en dos copias, una
  atrasada) — la copia atrasada se retira del control de versiones como parte de este cambio.
