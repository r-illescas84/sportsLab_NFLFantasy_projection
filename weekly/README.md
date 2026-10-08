# weekly/

Línea de trabajo activa del proyecto: proyecciones **semanales** por jugador (a diferencia de
`annual/`, que proyecta antes de que arranque la temporada y está en pausa — ver
`docs/decisions/0003-alcance-wr-semanal.md`).

## Estructura

Cada posición vive en su propia subcarpeta, con la misma forma general:

```
<posicion>/
  src/           # lo que corre cada semana: data.py -> features.py -> seguimiento.py -> modeling.py
                 # -> pipeline.py;
                 # experimentos.py y features_exploratorio.py solo los usan los notebooks
  notebooks/     # análisis por etapa, numerados etapa.paso (ver notebooks/README.md)
    _reference/  # notebooks originales, congelados: no se editan, solo se consultan
  models/        # los modelos que aplica el pipeline, con su metadata y sus límites de seguimiento
    exploratorio/  # modelos probados que no entraron al flujo semanal
  outputs/       # predicción emitida, variables, evaluación y métricas, por temporada y semana;
                 # tracking.csv, el historial de todas las corridas
```

## Posiciones

- **`wr/`** — la primera y, por ahora, la única completa de principio a fin (datos → análisis
  exploratorio → variables → modelado → evaluación → modelo final y flujo semanal). Es la
  referencia para adaptar el trabajo de las demás posiciones.

Al agregar una posición nueva, seguir la misma forma de carpetas, las mismas etapas de notebooks y
las mismas decisiones; los pasos, qué se cambia en el código y un punto de partida para QB, RB y TE
están en [`GUIA_NUEVA_POSICION.md`](GUIA_NUEVA_POSICION.md). Las decisiones que se repiten: split
temporal, comparación de modelos contra un baseline, métricas de selección consistentes con lo que se
predice
([`docs/decisions/0004-metricas-de-seleccion.md`](../docs/decisions/0004-metricas-de-seleccion.md)),
selección de variables con un orden estable y una regla de no inferioridad
([`docs/decisions/0005-seleccion-de-variables.md`](../docs/decisions/0005-seleccion-de-variables.md))
y seguimiento de cada corrida con límites de control
([`docs/decisions/0006-seguimiento-semanal.md`](../docs/decisions/0006-seguimiento-semanal.md)).

## Correr la semana (WR)

Dos veces por semana, desde la raíz del repositorio, dentro del contenedor del proyecto:

```bash
docker compose run --rm python python weekly/wr/src/pipeline.py --season 2026 --week 5
```

- **Antes del primer partido** (semana pendiente): guarda la predicción emitida
  (`wr/outputs/{temporada}/predicciones_semana_N.csv`) y las variables con que se hizo
  (`variables_semana_N.csv`). Si se vuelve a correr antes de los partidos, las dos se reemplazan.
- **Cuando terminó el último partido** (semana jugada): evalúa la predicción emitida sin modificarla
  (`evaluacion_semana_N.csv` y `metricas_semana_N.csv`) y compara las cuatro últimas semanas contra
  los límites de control guardados con cada modelo.
- **Con la semana a medias**, o si un chequeo de calidad invalida la predicción (un jugador repetido,
  una variable ausente o vacía, un equipo sin alineación, las estadísticas de la semana anterior sin
  cargar), se detiene sin escribir nada y dice por qué.

Cada corrida agrega una fila por resultado a `wr/outputs/tracking.csv`. Lo que hay que mirar:

- `valor` contra `referencia_validacion` y `referencia_prueba`: la métrica principal de la semana
  contra la del modelo al elegirlo.
- `alertas`: métricas de las cuatro últimas semanas fuera de sus límites. Una alerta suelta se anota
  y se vigila. Si dice «(persistente)», la misma métrica también estaba fuera cuatro semanas antes, y
  hay que revisar el modelo antes de la siguiente corrida.
- `advertencias`: chequeos de calidad que fallaron sin detener la corrida.

Se reentrena siempre al cerrar la temporada regular, y a mitad de temporada solo si una revisión
confirma una alerta persistente. En los dos casos se re-ejecuta
`wr/notebooks/6.1_modelo_final_y_temporada_actual.ipynb`, que también recalcula los límites; el
flujo semanal nunca reentrena solo. Las columnas se definen en el
[glosario](../docs/data/GLOSARIO_VARIABLES.md) (sección 10) y el ejemplo completo, con las semanas
de 2026, está en `wr/notebooks/6.3_seguimiento_semanal.ipynb`.
