# annual/

Proyecciones **anuales** (a nivel temporada completa), pensadas para correr antes de que
arranque la temporada de NFL, usando la temporada anterior como referencia.

**En pausa por decisión del equipo** (2026-09-28, ver
`docs/decisions/0003-alcance-wr-semanal.md`) — la temporada actual ya empezó, así que retomar
esto no tiene un uso real hasta la próxima entresemana. No está descartado permanentemente.

## Contenido

Código en R (`nflfastR`/`nflreadr`), organizado por posición: `qb/`, `rb/`, `te/`, `wr/`,
`dst/`, `k/`, más `rookies/`, `models/` y `extras/`. El entorno (`renv/`, `renv.lock`,
`.Rprofile`) vive en esta misma carpeta — para correr algo aquí, entrar primero con `cd annual`
antes de abrir R, ya que `renv` espera esos tres archivos juntos en el directorio de trabajo.

Hallazgos de calidad de datos y de código ya documentados en `docs/HALLAZGOS.md` (Fase 1) siguen
vigentes para cuando se retome este trabajo — no se repiten aquí.
