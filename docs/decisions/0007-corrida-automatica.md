# 0007 — Corrida semanal automática con GitHub Actions

**Fecha:** 2026-10-11
**Estado:** aceptada

## Contexto

Según el ADR 0006, el flujo semanal se corre a mano dos veces por semana: antes del primer partido,
para emitir la predicción, y después del último, para evaluarla. Si se olvida la emisión, esa semana
no tiene predicción emitida y solo se puede evaluar una reconstruida, que es justo lo que el ADR 0006
quiso evitar. Al planear las fases se eligió GitHub Actions para orquestar el flujo, pero esa decisión
no había quedado escrita.

## Decisión

### Dónde corre

Opciones: un cron en la computadora de alguien del equipo, que depende de que esté prendida; un
servicio externo; o **GitHub Actions** (elegida), que corre en los servidores de GitHub junto al
código y no cuesta en un repositorio público. El workflow es `.github/workflows/wr_semanal.yml`.

### Qué decide cada corrida

`python weekly/wr/src/pipeline.py --auto` decide con el calendario qué toca y lo corre con
`ejecutar_semana`, la misma función de la corrida manual:

- **Temporada:** de septiembre a diciembre, la del año; en enero, la anterior; de febrero a agosto no
  hay temporada regular y no hace nada. No se usa `nflreadpy.get_current_season()`: cambia de
  temporada el jueves después del Labor Day, y en 2026 la temporada empezó el miércoles 9 de
  septiembre.
- **Evaluar** cada semana jugada que todavía no tiene evaluación en `tracking.csv`, en orden.
- **Emitir** la primera semana pendiente, si la anterior ya se jugó (o es la semana 1) y su primer
  partido es dentro de cinco días. El límite de días es para la semana 1, que no tiene semana
  anterior: sin él se emitiría desde principios de septiembre, antes de que estén las alineaciones.
- Una semana en curso no se toca, ni se emite la siguiente, cuyas variables necesitan la semana
  anterior completa.

Las reglas están en `pipeline.acciones_pendientes`, una función que solo depende del calendario,
del historial y de la fecha, y se probó con calendarios sintéticos: el martes después de una semana,
el viernes, una semana que empieza en miércoles, la semana 1, la 18 y varias semanas atrasadas.

### Horario

Martes, miércoles y jueves a las 15:17 UTC (9:17 en la Ciudad de México), de septiembre a enero
(`17 15 * 1,9-12 2,3,4`). El martes evalúa la semana que terminó el lunes y emite la siguiente; el
miércoles y el jueves vuelven a emitirla con la alineación del día. Las semanas que empiezan en
miércoles (la 1 y la 12 de 2026) se emiten el martes. El minuto 17 evita el inicio de la hora, cuando
GitHub más retrasa las corridas programadas.

### Dónde quedan las salidas

Opciones: **commit directo a `main`** (elegida), una rama aparte para las salidas, o un PR por
corrida. Un PR por corrida obliga a alguien a aceptarlo dos veces por semana, que es lo que se quiere
evitar, y una rama aparte separa el historial de las salidas del código. El commit lo hace
`github-actions[bot]`, solo en `weekly/wr/outputs/` y solo si la corrida cambió algo.

### Avisos

Un issue en GitHub por cada evento que pide actuar, sin repetir uno que ya esté abierto con el mismo
título:

- **Corrida detenida:** un chequeo de calidad bloqueó la semana (ADR 0006). No se escribe nada y la
  corrida sale con el código 3.
- **Estadísticas incompletas:** al evaluar, algún equipo que jugó no tiene receptores en la tabla.
- **Alerta persistente:** hay que revisar el modelo antes de la siguiente corrida (ADR 0006).
- **Fin de la temporada regular:** se evaluó la semana 18 y toca reentrenar con
  `6.1_modelo_final_y_temporada_actual.ipynb`.

Cualquier otro error hace fallar el workflow, y GitHub manda un correo.

### Estadísticas que llegan tarde

Se consideró que «cada equipo que juega tiene receptores» también bloqueara en semanas jugadas, para
que la corrida del día siguiente reintentara. **No se adoptó:** no se sabe si la fuente publica las
estadísticas de todos los partidos de una semana al mismo tiempo, y bloquear podría dejar una semana
sin evaluar por algo que no invalida lo demás. Se evalúa con lo disponible, como en la corrida
manual, y se abre el issue de estadísticas incompletas para volver a correr esa semana cuando estén;
la nueva evaluación reemplaza a la anterior en las tablas y las alertas. En las 179 semanas jugadas
de 2016 a la 4 de 2026, ningún equipo que jugó quedó sin receptores, salvo por los códigos de época
del calendario (OAK de 2016 a 2019 y SD en 2016), que no afectan a temporadas nuevas.

### La misma imagen que en local

El workflow construye `docker/Dockerfile.python`, con las versiones fijadas de
`docker/requirements.txt`, y guarda las capas en la caché de GitHub. Con otras versiones de xgboost o
scikit-learn, los modelos guardados podrían no cargar o predecir distinto. Los datos se descargan
frescos en cada corrida.

## Consecuencias

- `pipeline.py`: el modo `--auto` y las opciones `--resumen` (markdown que el workflow muestra en la
  corrida y usa como mensaje del commit) y `--eventos` (JSON con los issues por abrir). Un chequeo
  bloqueante sale con el código 3 también en la corrida manual.
- El bot hace commits en `main`: antes de subir algo hay que traer lo último con `git pull`.
- El workflow pide permiso de escritura en el contenido y en los issues. Si la configuración del
  repositorio lo restringe, quien lo administra tiene que permitirlo (Settings > Actions > General >
  Workflow permissions).
- GitHub puede retrasar o, con mucha carga, saltarse una corrida programada; las del miércoles y el
  jueves cubren a la del martes. En un repositorio público, GitHub desactiva los workflows
  programados tras 60 días sin actividad, lo que puede pasar entre febrero y agosto: antes de
  septiembre hay que revisar que el workflow siga activo.
- Se puede seguir corriendo a mano: en local, como hasta ahora, o en GitHub (Actions > Corrida
  semanal WR > Run workflow), con temporada y semana o sin ellas. Con `publicar` apagado no sube nada
  ni abre issues, y sirve de prueba.
- Un partido cancelado o movido fuera de su semana la deja en curso: `--auto` no la evalúa ni emite
  la siguiente, y esas semanas se corren a mano.
- Fuera de alcance: las otras posiciones, que agregan su paso al workflow cuando existan
  (`weekly/GUIA_NUEVA_POSICION.md`), y reentrenar, que sigue pasando siempre por 6.1. La emisión
  sigue fijándose con el primer partido de la semana (ADR 0006): los cambios de alineación posteriores
  de los equipos que juegan el domingo no entran.

## Referencias

- GitHub Docs. *Events that trigger workflows: schedule*. Las corridas programadas usan la última
  versión de la rama principal, pueden retrasarse con mucha carga (sobre todo al inicio de cada hora) y
  se desactivan en repositorios públicos tras 60 días sin actividad.
