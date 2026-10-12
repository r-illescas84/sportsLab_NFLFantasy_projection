# 0005 — Selección de variables estable

**Fecha:** 2026-10-07
**Estado:** aceptada

## Contexto

Los modelos usan 34 variables elegidas en `3.2_seleccion_features.ipynb`: la unión de las 15 más
importantes de cada resultado, más edad y experiencia. Esa selección tiene tres debilidades
registradas en `docs/HALLAZGOS.md`:

- Midió la importancia con MAE, que premia la mediana (ADR 0004).
- Usó 2024-2025 como validación, los mismos años que la etapa 4 usa como prueba: la prueba no es
  del todo independiente de las variables elegidas.
- Es inestable: con el dato de alineaciones corregido, la misma regla daría 31 variables en lugar
  de 34.

Además, el total implícito de las líneas de apuestas quedó pendiente de evaluar como candidata
(`2.3_eda_general.ipynb`).

Las candidatas están muy correlacionadas entre sí. Al agrupar las de entrenamiento (2016-2021) por
correlación de Spearman, sin mirar los resultados, casi todas las variables de volumen y
participación (recepciones, objetivos, yardas, primeros downs, `target_share`, `wopr`, puntos de
fantasy, en sus cuatro ventanas) forman un solo grupo de 53 variables con correlación media de 0.7
o más. Con variables así, la importancia por permutación de cada una se reparte entre las demás y
una sola corrida no dice cuáles hacen falta.

## Opciones consideradas

1. **Repetir la regla del top 15 con la métrica principal y la validación correcta** — corrige las
   dos primeras debilidades, pero no la inestabilidad ni el reparto de importancia.
2. **Permutar por grupos de variables correlacionadas** — evita el reparto, pero el grupo de 53
   variables quedaría como una sola unidad y no diría cuáles de ellas usar.
3. **Eliminación recursiva con importancia por permutación, repetida en submuestras** — al quitar en
   rondas las variables más débiles y recalcular la importancia, cuando sale una de dos variables
   casi iguales la otra recupera su importancia (Gregorutti, Michel y Saint-Pierre, 2017). Repetirla
   en submuestras mide qué tan estable es el orden (en el espíritu de Meinshausen y Bühlmann, 2010).

## Decisión

Opción 3, en `3.3_seleccion_estable.ipynb`, con estas reglas fijadas antes de ver los resultados:

- **Candidatas:** las 109 de `3.2_seleccion_features.ipynb` (21 estadísticas en cuatro ventanas,
  más perfil, contexto de equipo y de partido, volatilidad, cambios e interacciones), con la
  alineación unificada (incluye 2025), más el total implícito de puntos del equipo. Las categóricas
  (`roof`, `surface`, `anios_experiencia_bucket`) entran como una unidad cada una.
- **Modelo:** el XGBoost de cada resultado con la configuración vigente al seleccionar, la elegida
  con las 34 variables anteriores (en touchdowns, 200 árboles con tasa 0.08). La importancia se mide
  con la métrica principal de cada resultado (ADR 0004), como aumento relativo de la pérdida al
  permutar la variable.
- **Orden sin tocar la validación:** se ajusta con 2016-2019 y la importancia se mide en 2020-2021.
  El conjunto es compartido por los tres resultados, así que cada variable se califica con su mayor
  importancia relativa entre los tres. En cada ronda sale el 10% de las variables con menor
  calificación (al menos una) y se recalcula la importancia con las que quedan.
- **Estabilidad:** la eliminación se repite en 10 submuestras de la mitad de las semanas de
  2016-2019. Las variables se ordenan por la ronda promedio en que salen: las que duran más van
  primero.
- **Tamaño, en validación (2022-2023):** se ajustan con 2016-2021 los conjuntos de las 5, 10, 15,
  20, 25, 30, 40, 50 y 75 primeras variables, todas las candidatas y las 34 actuales. Para cada
  resultado, el mejor es el de menor pérdida, y cada conjunto se compara contra él con el bootstrap
  de semanas del ADR 0004. Se elige el conjunto más chico que no sea más de 1% peor que el mejor en
  ninguno de los tres resultados, con 95% de confianza: la cota superior del intervalo de la
  diferencia no debe pasar de 1% de la pérdida del mejor (prueba de no inferioridad con margen).
- **La prueba (2024-2025) solo se reporta.**

### Corrección de la regla de tamaño

La primera versión elegía el conjunto más chico «empatado» con el mejor y con las 34 actuales:
intervalo de la diferencia que incluyera cero, con el nivel ajustado por Bonferroni por 30
comparaciones (99.83%). Un empate así solo dice que la diferencia no se distinguió del ruido, no que
el conjunto no sea peor, y favorece a los conjuntos cuyas diferencias tienen más ruido; el ajuste,
al ensanchar los intervalos, lo agrava. Esa regla elegía las 10 primeras variables, mientras que los
conjuntos de 15 a 40 no pasaban. El problema se hizo visible al reportar la prueba, donde ese conjunto
quedaba peor que las 34 en recepciones. Se corrigió por principio, con la prueba de no inferioridad de
arriba aplicada solo en validación, y se deja registro de que la prueba ya se había visto. La
verificación sin ningún uso previo es la temporada 2026.

### Resultado (`3.3_seleccion_estable.ipynb`)

- **25 variables.** Con ellas la pérdida queda como mucho a 0.86% del mejor en recepciones, 0.41% en
  yardas y 0.90% en touchdowns. Con 10, 15 o 20 la cota en touchdowns pasa de 1% (1.55%, 1.45% y
  1.38%); las 34 actuales tampoco cumplen (1.69% en touchdowns).
- Contra las 34 actuales, en validación empatan en los tres resultados. En la prueba quedan peor en
  recepciones (+0.0097 de RMSE, intervalo de +0.0044 a +0.0151) y empatan en yardas y touchdowns;
  esa comparación favorece a las 34, que se eligieron midiendo la importancia en los años de prueba.
- Con las 25 variables, la etapa 4 llega después a otra configuración de touchdowns (400 árboles con
  tasa 0.03). Repetir la selección con ella da el mismo tamaño y cambia una variable en el borde del
  orden: entra `targets_last3_avg` y sale `air_yards_share_last3_avg`. Las 25 adoptadas también pasan
  la regla, con pérdidas de validación prácticamente iguales, y se mantienen (sección 7 de 3.3).
- Entran el total implícito (octavo del orden estable), las yardas tras la recepción, la interacción
  de participación con la ofensiva del equipo y otras ventanas de las mismas estadísticas; salen,
  entre otras, la experiencia, el EPA, la tasa de atrapadas y las jugadas de 10, 16 y 20 yardas o más.

## Consecuencias

- `features_exploratorio.py`: la tabla de candidatas y el total implícito, para no repetir en el
  notebook la construcción de 3.2. `experimentos.py`: importancia por permutación con la métrica
  principal, eliminación recursiva y orden estable.
- `features.py` adopta las 25 (`FEATURES_SELECCIONADAS_ARBOL`, en el orden estable); el total
  implícito, la ofensiva del equipo y su interacción pasan al flujo semanal. El conjunto para modelos
  lineales se deriva del nuevo: sin las versiones de `target_share` y `air_yards_share` (queda `wopr`)
  y con la experiencia en versión no lineal. Las etapas 4 a 6 se vuelven a ejecutar con el mismo
  procedimiento; `4.4_ajuste_hiperparametros.ipynb` deja de probar conjuntos propios por resultado,
  porque el conjunto compartido es parte de esta decisión.
- El total implícito, si entra, se calcula en el flujo semanal con la línea vigente al momento de
  predecir; las líneas históricas del calendario son las de cierre.
- `3.2_seleccion_features.ipynb` queda como registro de la selección anterior, y sus 34 variables
  en `features_exploratorio.VARIABLES_SELECCION_3_2`.

## Referencias

- Breiman, L. (2001). Random forests. *Machine Learning*, 45(1), 5-32.
- Strobl, C., Boulesteix, A.-L., Kneib, T., Augustin, T. y Zeileis, A. (2008). Conditional variable
  importance for random forests. *BMC Bioinformatics*, 9, 307.
- Meinshausen, N. y Bühlmann, P. (2010). Stability selection. *Journal of the Royal Statistical
  Society: Series B*, 72(4), 417-473.
- Gregorutti, B., Michel, B. y Saint-Pierre, P. (2017). Correlation and variable importance in
  random forests. *Statistics and Computing*, 27(3), 659-678.
