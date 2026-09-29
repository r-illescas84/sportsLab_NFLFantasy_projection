# weekly/

Línea de trabajo activa del proyecto: proyecciones **semanales** por jugador (a diferencia de
`annual/`, que proyecta antes de que arranque la temporada y está en pausa — ver
`docs/decisions/0003-alcance-wr-semanal.md`).

## Estructura

Cada posición vive en su propia subcarpeta, con la misma forma general:

```
<posicion>/
  notebooks/   # un notebook por target a predecir, más uno que orquesta el merge
  outputs/     # predicciones ya generadas, por año y semana
  data/        # snapshots propios de los datos descargados (no el dato "en vivo" sin guardar)
  models/      # modelos entrenados + una Model Card por target
  docs/
    tracking.csv   # una fila por corrida: fecha, modelo, target, métricas, ventana de datos
```

## Posiciones

- **`wr/`** — la primera y, por ahora, la única completa de principio a fin (datos → EDA →
  features → modelo comparado → tracking). Es la referencia para adaptar el trabajo de las
  demás posiciones cuando llegue.

Al agregar una posición nueva, seguir la misma forma de carpetas y el mismo patrón de pipeline
que `wr/` — no es necesario copiar su código línea por línea, sí su estructura y sus decisiones
(split temporal, comparación de modelos con baseline, tracking de métricas por corrida).
