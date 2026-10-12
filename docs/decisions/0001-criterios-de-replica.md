# 0001 — Criterios de éxito para la réplica (Fase 1)

**Fecha:** 2026-09-28
**Estado:** aceptada

## Contexto

Antes de tocar cualquier bug o hacer cambios, la Fase 1 corre el pipeline tal cual (sin
modificar lógica) dentro del entorno Docker, y compara la salida contra los archivos ya
publicados en el repo. Hacía falta definir qué cuenta como "coincide" con números concretos,
no una noción vaga de "parecido".

## Decisión

Umbrales por tipo de columna:

| Tipo de columna | Ejemplos | Métrica | Umbral |
|---|---|---|---|
| Llaves/categóricas | `player_id`, `season`, `week`, `game_id`, `Team`, `POS` | % de filas con match exacto | 100% |
| Conteos (enteros) | `targets`, `completions`, `receptions` | diferencia exacta | 0 |
| Promedios/ratios calculados | `*_season_avg`, `*_career_avg`, `*_last5`, `rec_ypg`, `target_share` | diferencia absoluta | < 1e-6 |
| Predicciones XGBoost | `predicted_receiving_yards`, etc. | diferencia relativa | < 1% (piso absoluto 0.05 para valores cercanos a cero) |
| Puntos fantasy finales | `projected_ppr_points`, `projected_std_points` | diferencia relativa | < 1% (mismo piso 0.05) |

Por columna se reporta MAE + % de filas fuera de umbral. Pasa si ≥99.5% de las filas están
dentro del umbral. Cualquier columna que falle se investiga antes de avanzar — no se documenta
y se sigue adelante sin más.

**Caso especial — `career_avg`/`last5_avg`:** el bug de fuga de información ya está confirmado
(ver notebook `06_capa_semanal_y_un_bug_real.ipynb` fuera de este repo). Aquí el éxito no es
"coincide con el valor correcto", es "reproduce el mismo error": el % de jugadores con el
patrón de arrastre en su primer partido debe caer entre 90–94% (el original documentó ~92% de
~2,100 jugadores). Si el bug no aparece en la réplica, es señal de alarma — algo cambió en el
entorno sin que lo notemos.

## Consecuencias

Estos umbrales se usan tal cual al comparar cada etapa del pipeline en la Fase 1. Si algún
umbral resulta poco práctico durante la ejecución real, se documenta un nuevo ADR ajustándolo
— no se cambia en silencio.
