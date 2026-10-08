# weekly/

Línea de trabajo activa del proyecto: proyecciones **semanales** por jugador (a diferencia de
`annual/`, que proyecta antes de que arranque la temporada y está en pausa — ver
`docs/decisions/0003-alcance-wr-semanal.md`).

## Estructura

Cada posición vive en su propia subcarpeta, con la misma forma general:

```
<posicion>/
  src/           # lo que corre cada semana: data.py -> features.py -> modeling.py -> pipeline.py;
                 # experimentos.py y features_exploratorio.py solo los usan los notebooks
  notebooks/     # análisis por etapa, numerados etapa.paso (ver notebooks/README.md)
    _reference/  # notebooks originales, congelados: no se editan, solo se consultan
  models/        # los modelos que aplica el pipeline, con su metadata
    exploratorio/  # modelos probados que no entraron al flujo semanal
  outputs/       # predicciones y métricas generadas, por temporada y semana
```

## Posiciones

- **`wr/`** — la primera y, por ahora, la única completa de principio a fin (datos → análisis
  exploratorio → variables → modelado → evaluación → modelo final y flujo semanal). Es la
  referencia para adaptar el trabajo de las demás posiciones.

Al agregar una posición nueva, seguir la misma forma de carpetas, las mismas etapas de notebooks y
las mismas decisiones: split temporal, comparación de modelos contra un baseline, métricas de
selección consistentes con lo que se predice
([`docs/decisions/0004-metricas-de-seleccion.md`](../docs/decisions/0004-metricas-de-seleccion.md))
y selección de variables con un orden estable y una regla de no inferioridad
([`docs/decisions/0005-seleccion-de-variables.md`](../docs/decisions/0005-seleccion-de-variables.md)).
