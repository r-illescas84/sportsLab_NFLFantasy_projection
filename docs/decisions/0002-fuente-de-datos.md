# 0002 — Fuente de datos: `nflverse`, sin scouting propietario

**Fecha:** 2026-09-28
**Estado:** aceptada

## Contexto

Antes de rediseñar features y modelo (Fase 2 del plan de consolidación), hacía falta confirmar
si `nflverse` (`nfl_data_py`/`nflfastR`) es la única fuente razonable, o si el proyecto se está
quedando corto por no usar algo más completo. Ver el detalle de la auditoría en
[`../data/DATASHEET.md`](../data/DATASHEET.md).

## Opciones consideradas

1. **`nflverse`** (actual) — abierto, gratuito, *play-by-play* desde 1999, incluye líneas de
   Vegas y clima dentro de `import_schedules()`, y métricas de eficiencia (EPA, `target_share`,
   `wopr`) dentro de `import_weekly_data()` — ya disponibles, hoy sin usar.
2. **Sportradar / Stats Perform / SIS** — datos de scouting propietarios (grades de jugador,
   tracking de posicionamiento). Es la ventaja real que menciona PFF en su metodología pública
   ("PFF+ grades"). De paga, con contratos anuales típicamente fuera del alcance de un proyecto
   de este tamaño.
3. **APIs de casas de apuestas en vivo** — líneas actualizadas al minuto (no solo cierre).
   `nflverse` ya cubre el caso de uso principal (línea de cierre) sin costo ni integración nueva.

## Decisión

Seguir con `nflverse` como única fuente por ahora. No se justifica pagar por scouting
propietario en esta etapa — el diagnóstico de la Fase 2 (ver Datasheet) muestra que el problema
actual no es falta de fuente, es que el pipeline **no usa** buena parte de lo que la fuente
actual ya ofrece (Vegas, clima, EPA).

## Consecuencias

- La Fase 3 (feature engineering) se enfoca en aprovechar lo ya disponible en `nflverse` antes de
  considerar cualquier fuente nueva.
- Si más adelante el techo de mejora se agota con `nflverse` solo, evaluar scouting propietario
  como una decisión aparte, con su propio ADR — no se descarta para siempre, se pospone con
  criterio.
