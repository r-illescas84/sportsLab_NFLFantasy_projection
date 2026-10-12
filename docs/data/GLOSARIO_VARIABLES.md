# Glosario de variables — pipeline WR semanal

Guía para entender cada variable del proyecto sin necesidad de conocer fútbol americano: qué mide, de qué fuente viene, cómo se calcula, un ejemplo real y si el modelo la usa. Cubre las cuatro tablas de datos (`nflreadpy`), las variables que construye `features.py`, las 25 que usan los modelos y las que se probaron y se descartaron.

Las definiciones de las columnas de las tablas de origen salen de los [diccionarios oficiales de `nflverse`](https://github.com/nflverse/nflreadr/tree/main/data-raw), traducidas. Donde la fuente no define una columna, se indica y se explica cómo se comprobó su significado.

## Cómo leer este documento

**Una fila = un receptor (WR) en un partido de temporada regular.** A eso se le llama *jugador-semana*. La tabla con la que se entrenan los modelos tiene 23,610 filas (2016–2025, 700 receptores distintos) y 220 columnas.

**Todos los ejemplos salen del mismo partido real**, para poder seguir un solo caso de principio a fin:

|  | Dato |
|---|---|
| Jugador | Ja'Marr Chase, receptor (WR) de CIN |
| Partido | Semana 13 de 2025, jueves 2025-11-27: BAL (local) contra CIN (visitante) |
| Marcador | CIN 32 – BAL 14 |
| Lo que hizo Chase | 14 objetivos, 7 recepciones, 110 yardas, 0 touchdowns |

**Corte de las cifras:** rangos y ejemplos salen de los datos 2016–2025; los pesos de la sección 8, de los modelos guardados el 2026-10-07. Si se reentrenan los modelos o cambian los datos, hay que recalcularlos.

**Etiquetas de la columna «Dónde se usa»:**

| Etiqueta | Significado |
|---|---|
| **Resultado** | Es lo que los modelos predicen. |
| **Insumo** | No entra tal cual al modelo; de ella se calculan variables que sí entran. |
| **Modelo** | Una de las 25 variables que usan los 3 modelos vigentes. |
| **Evaluada** | Se construyó y probó en los notebooks 3.1 a 3.3 y no entró al modelo. |
| **Fuente** | Viene en las tablas de `nflreadpy`; el pipeline semanal no la usa. |

**Regla que explica casi todo el diseño:** los modelos nunca ven lo que pasó en el partido que predicen. Para predecir la semana 13 solo usan lo ocurrido en semanas anteriores (por eso las variables del modelo son promedios del pasado, ver sección 7). Si se colara un dato del propio partido, el modelo parecería muy bueno en pruebas y fallaría al predecir una semana que aún no se juega.

## 1. Posiciones y alineación

El proyecto predice **receptores (WR)**. Las tablas de origen traen a todos los jugadores de la liga, así que aparecen otras posiciones; `data.py` y `features.py` filtran `position == "WR"`.

| Código | Nombre | Qué hace | Unidad |
|---|---|---|---|
| QB | Mariscal de campo (quarterback) | Lanza los pases y dirige la ofensiva. | Ataque |
| RB / FB | Corredor (running back) / fullback | Corre con el balón; el fullback bloquea para él. | Ataque |
| WR | Receptor abierto (wide receiver) | Corre rutas hacia afuera del centro del campo y recibe pases. **Es la posición de este proyecto.** | Ataque |
| TE | Ala cerrada (tight end) | Híbrido: bloquea como lineal y recibe pases como receptor. | Ataque |
| OL: C, G, OT, OL | Línea ofensiva: centro, guardias, tackles | Bloquean para proteger al QB y abrir paso al corredor. No suelen registrar estadísticas de balón. | Ataque |
| DL: DE, DT, NT, DL | Línea defensiva: ends, tackles, nose tackle | Presionan al QB y frenan las carreras. | Defensa |
| LB: ILB, MLB, OLB, LB | Apoyadores (linebackers) | Defienden la carrera, cubren pases cortos y presionan al QB. | Defensa |
| DB: CB, S, FS, SAF, DB | Backs defensivos: esquineros y safeties | Cubren a los receptores y evitan pases largos. | Defensa |
| K | Pateador (kicker) | Patea goles de campo y puntos extra. | Equipos especiales |
| P | Despejador (punter) | Despeja el balón al rival cuando el equipo cede la posesión. | Equipos especiales |
| LS | Snapper largo (long snapper) | Hace el pase atrás en despejes y goles de campo. | Equipos especiales |

Filas por posición en la tabla de estadísticas (174,376 filas, 2016–2025): WR 23,610, LB 17,962, CB 17,240, RB 14,884, DE 14,754, DT 13,245, TE 11,778, DB 6,948, QB 6,282, SAF 6,170, OLB 5,704, K 5,273, P 5,221, OT 4,224, FS 4,140, S 3,599, G 3,100, ILB 2,827, MLB 2,147, C 1,495, NT 1,363, FB 1,215, LS 686, DL 305, OL 29. El código exacto de cada jugador viene en `position`; `position_group` lo agrupa en DB, DL, LB, WR, RB, TE, SPEC, OL, QB.

### Alineación (depth chart)

Es la lista que publica cada equipo con quién ocupa cada lugar de una posición: el titular, el suplente, etc. En el proyecto se llama `depth_team` (1 = titular, o «WR1»; 2 = «WR2»; y así). Ejemplo, la alineación de WR de CIN para esa semana:

| depth_team | Jugador | ¿Tiene fila en la tabla de estadísticas esa semana? |
|---|---|---|
| 1 | Ja'Marr Chase | sí |
| 2 | Tee Higgins | no |
| 3 | Andrei Iosivas | sí |
| 4 | Mitchell Tinsley | sí |
| 5 | Charlie Jones | sí |
| 6 | Jermaine Burton | no |

Dos cosas que conviene saber: (1) puede haber un jugador en la alineación sin fila de estadísticas ese partido (arriba, el lugar 2); esa fila simplemente no entra a la tabla de modelado. (2) Los lugares no son consecutivos para quienes sí tienen fila (aquí 1, 3, 4, 5).

## 2. Conceptos de juego que aparecen en las variables

| Concepto | Qué es | En el ejemplo (Ja'Marr Chase, semana 13) |
|---|---|---|
| Temporada regular y semana | Cada equipo juega 1 partido por semana, con una semana de descanso (*bye*). Hay 18 semanas desde 2021 (17 de 2016 a 2020) y cada equipo juega 17 partidos. | CIN no tuvo partido en la semana 10 de 2025 (su descanso). |
| Objetivo (*target*) | Pase dirigido a un receptor, lo atrape o no. Mide cuánto lo buscan. | Le lanzaron 14 pases. |
| Recepción | Pase que el receptor atrapa. | Atrapó 7 de 14. |
| Yardas aéreas (*air yards*) | Distancia, medida desde la línea de golpeo (donde empieza la jugada), hasta el punto donde el receptor atrapó o no el balón. Cuenta también pases incompletos y es negativa si el pase queda detrás de esa línea. Mide qué tan profundo lo buscan, no cuánto gana al final. | Sus 14 objetivos sumaron 171 yardas aéreas. |
| Yardas después de la recepción (YAC) | Yardas que se ganan desde el punto de la recepción hasta donde termina la jugada. | 9 yardas. |
| Primer down | Avance suficiente para ganar otra serie de 4 jugadas. Una recepción que da primer down mantiene viva la ofensiva. | 5 primeros downs por recepción. |
| Touchdown (TD) | Anotación de 6 puntos. En `receiving_tds`, la que sigue a una recepción. | 0. |
| EPA (puntos esperados agregados) | Mide cuánto cambia una jugada la cantidad de puntos que se espera que anote el equipo. Positivo = la jugada ayudó; negativo = perjudicó. `receiving_epa` suma el EPA de las jugadas en que fue objetivo. | 7.26. |
| Puntos de fantasy (PPR) | Puntaje de las ligas de fantasy football. «PPR» da 1 punto extra por recepción. | 18 = 11 estándar + 7 recepciones. |
| Alineación (*depth chart*) | Lista de titulares y suplentes por posición. Ver sección 1. | `depth_team` = 1. |
| Línea de apuestas (*spread*) | Ventaja en puntos que las casas de apuestas dan a un equipo. Positiva = favorito el local. | 7.0: BAL favorito por 7. |

## 3. Las cuatro fuentes de datos

Todo viene de **`nflreadpy`**, la librería de Python del ecosistema abierto `nflverse` (ver [DATASHEET.md](DATASHEET.md) para su auditoría de calidad). `data.py` las carga y limpia; cada función consulta la fuente directamente y filtra a temporada regular.

| Tabla | Función de `nflreadpy` | Función de `data.py` | Tamaño usado | Una fila es… | Qué toma el pipeline |
|---|---|---|---|---|---|
| **Estadísticas semanales** | `load_player_stats(summary_level="week")` | `cargar_stats_semanales` | 174,376 × 150 (todas las posiciones, 2016–2025; 23,610 filas son WR) | un jugador en un partido | Las columnas de recepción (sección 6), los identificadores (sección 4) y los 3 resultados. |
| **Jugadores** | `load_players()` | `cargar_jugadores` | 24,844 × 39 | un jugador (histórico) | `birth_date` (edad), `rookie_season` (experiencia), `draft_pick`, `display_name`. |
| **Calendario** | `load_schedules(seasons)` | `cargar_calendario` | 2,639 × 46 (2016–2025, solo temporada regular) | un partido | Fecha de cada partido (para asignar la alineación a su semana) y marcador (para saber si la semana ya se jugó). Más contexto probado y descartado, sección 9. |
| **Alineaciones** | `load_depth_charts(seasons)` | `cargar_depth_charts_unificado` | 32,698 × 5 (2016–2026, ya unificada) | un jugador en la alineación de su equipo esa semana | `depth_team`. |
| Jugadas (*play-by-play*) | `load_pbp(seasons)` | `cargar_jugadas` | no se usa en el flujo semanal | una jugada | Nada: lo que antes se calculaba desde aquí (`target_share` y otros) ya viene en la tabla de estadísticas. |

**Cómo se unen:** estadísticas, jugadores y alineaciones comparten el identificador del jugador (`player_id`, que en las otras dos tablas se llama `gsis_id`). Estadísticas y alineaciones se unen además por `(season, week, team)`; como las alineaciones históricas usan el código de equipo de la época (`OAK`, `SD`) y las estadísticas el actual (`LV`, `LAC`), los códigos viejos se traducen antes de unir. El calendario no se une a la tabla de modelado: solo sirve para ubicar en el tiempo a la alineación y para saber si una semana ya se jugó.

**Alineaciones, un detalle importante.** `nflreadpy` publica dos esquemas incompatibles: hasta 2024 trae temporada y semana, de 2025 en adelante solo la fecha y hora en que se capturó la lista. Para unificarlos, cada captura se asigna al *próximo* partido de ese equipo (una alineación describe lo que el equipo prepara, no lo que ya jugó) y, si hay varias capturas de la misma semana, se queda la más reciente. Resultado: de 23,610 filas WR históricas, 92.5 % tienen `depth_team`. El resto son, sobre todo, receptores que esa semana no aparecen en la alineación de ningún grupo; una parte menor aparece en otro lugar (regresador de patadas, otra posición o una etiqueta distinta de «WR»).

## 4. Identificadores y contexto de la fila

| Variable | Qué es | Ejemplo | Fuente | Dónde se usa |
|---|---|---|---|---|
| `player_id` | Identificador único del jugador (`gsis_id` de la NFL). Es la llave para unir tablas. | 00-0036900 | Estadísticas | Llave; no entra al modelo |
| `player_name` | Nombre abreviado, como lo entrega la API de estadísticas de la NFL. | J.Chase | Estadísticas | Solo para mostrar |
| `player_display_name` | Nombre completo (el de `load_players()`). | Ja'Marr Chase | Estadísticas | Se muestra en la salida |
| `position` | Posición del jugador según la NFL (sección 1). | WR | Estadísticas | Se filtra a `WR` |
| `position_group` | Grupo de posición según la NFL. | WR | Estadísticas | Fuente |
| `team` | Equipo del jugador (abreviatura). | CIN | Estadísticas | Se muestra y une con la alineación |
| `opponent_team` | Equipo rival ese partido. | BAL | Estadísticas | Fuente |
| `season` | Año de la temporada NFL (una temporada que termina en enero cuenta con el año en que empezó). | 2025 | Estadísticas | Llave temporal |
| `week` | Semana de la temporada, 1 a 18. | 13 | Estadísticas | Llave temporal |
| `season_type` | `REG` temporada regular, `POST` playoffs. `data.py` deja solo `REG`. | REG | Estadísticas | Filtro |
| `game_id` | Identificador del partido: temporada, semana, visitante y local. | 2025_13_CIN_BAL | Estadísticas y calendario | Fuente |
| `headshot_url` | Enlace a la foto oficial del jugador. | URL | Estadísticas | Fuente |
| `es_prediccion` | Marca que pone el pipeline: `True` en las filas de una semana todavía no jugada (sus estadísticas aún no existen), `False` en las reales. | False | Calculada | Separa filas a predecir de filas reales |

## 5. Los tres resultados que se predicen

| Resultado | Qué es | Ejemplo | Media · mediana · máximo | % de partidos en 0 |
|---|---|---|---|---|
| `receptions` | Pases que atrapó en el partido. | 7 | 2.698 · 2 · 18 | 21.9 % |
| `receiving_yards` | Yardas ganadas en recepciones. Puede ser negativa (recibir y perder terreno). | 110 | 34.278 · 23 · 300 (mínimo -13) | 22.2 % |
| `receiving_tds` | Touchdowns tras una recepción. | 0 | 0.209 · 0 · 4 | 81.8 % |

Los touchdowns son un evento raro (en 81.8 % de los partidos el receptor no anota), por eso es el resultado más difícil de predecir y el que usa una función de pérdida distinta (Poisson).

Los tres son **Resultado** y a la vez **Insumo**: el valor de un partido pasa a formar parte del promedio con el que se predicen los partidos *siguientes*.

## 6. Estadísticas semanales de recepción (la tabla de estadísticas)

Valores de **un solo partido**. El modelo no los usa directamente: toma sus promedios pasados (sección 7). Los rangos son los observados en toda la tabla de modelado: mínimo · mediana · máximo.

| Variable | Qué mide | Ejemplo (Ja'Marr Chase, s. 13) | Rango observado | Dónde se usa |
|---|---|---|---|---|
| `receptions` | Pases atrapados. | 7 | 0 · 2 · 18 | **Resultado.** Insumo: entra al modelo como `receptions_career_avg`, `receptions_season_avg`, `receptions_last3_avg`, `receptions_last5_avg` |
| `targets` | Veces que le lanzaron el balón (objetivos). | 14 | 0 · 4 · 23 | Insumo: entra al modelo como `targets_season_avg`, `targets_last5_avg` |
| `receiving_yards` | Yardas ganadas en recepciones. | 110 | -13 · 23 · 300 | **Resultado.** Insumo: entra al modelo como `receiving_yards_career_avg`, `receiving_yards_last5_avg` |
| `receiving_air_yards` | Yardas aéreas de todos sus objetivos, atrapados o no (sección 2). | 171 | -32 · 35 · 334 | Evaluada (notebooks 3.2 y 3.3); no entró al modelo |
| `receiving_first_downs` | Primeros downs ganados por recepción. | 5 | 0 · 1 · 14 | Insumo: entra al modelo como `receiving_first_downs_career_avg` |
| `receiving_tds` | Touchdowns tras recepción. | 0 | 0 · 0 · 4 | **Resultado.** Sus promedios solo los usa la referencia de los notebooks; no entra al modelo como variable |
| `receiving_10` | Recepciones que ganaron **10 o más yardas**. La fuente no la define; se verificó contra las jugadas de 2024: coincide con «recepciones de ≥10 yardas» en 99.67 % de los partidos de WR. | 5 | 0 · 1 · 10 | Evaluada (notebooks 3.2 y 3.3); no entró al modelo |
| `receiving_16` | Recepciones de **16 o más yardas** (misma verificación: 99.63 %). | 1 | 0 · 0 · 7 | Evaluada (notebooks 3.2 y 3.3); no entró al modelo |
| `receiving_20` | Recepciones de **20 o más yardas** (misma verificación: 99.75 %). | 1 | 0 · 0 · 7 | Evaluada (notebooks 3.2 y 3.3); no entró al modelo |
| `receiving_40` | Recepciones de **40 o más yardas** («jugada larga»; verificación 99.92 %). | 1 | 0 · 0 · 3 | Evaluada (notebooks 3.2 y 3.3); no entró al modelo |
| `target_share` | Participación: qué fracción de los objetivos de su equipo recibió él. Se reproduce exactamente como sus objetivos entre la suma de objetivos de todos los jugadores del equipo (coincide en 100.0 % de las filas WR). | 0.318 (14 de 44) | 0.000 · 0.114 · 0.667 | Insumo: entra al modelo como `target_share_last5_avg`, `target_share_last3_avg`, `target_share_season_avg` y dentro de `target_share_x_ofensiva_equipo` |
| `air_yards_share` | Participación en yardas aéreas: qué fracción de las yardas aéreas de su equipo le tocaron. Se usa tal como la publica la fuente: al reconstruirla sumando las yardas aéreas de los jugadores de la tabla solo coincide en 52.53 % de las filas, así que el denominador de la fuente no es esa suma. Puede ser negativa cuando sus yardas aéreas del partido fueron negativas. | 0.450 (45 %) | -0.423 · 0.140 · 1.842 | Insumo: entra al modelo como `air_yards_share_last3_avg` |
| `wopr` | *Weighted OPportunity Rating*: resume en un número cuánto lo usa el equipo, combinando objetivos y profundidad. Fórmula oficial: 1.5 × `target_share` + 0.7 × `air_yards_share`; se comprobó exacta en las 23,610 filas WR (diferencia máxima: 0). | 0.792 = 1.5 × 0.318 + 0.7 × 0.450 | -0.101 · 0.277 · 1.789 | Insumo: entra al modelo como `wopr_last3_avg`, `wopr_season_avg` |
| `receiving_epa` | Puntos esperados agregados sumados sobre las jugadas en que fue objetivo (sección 2). Vacío cuando no tuvo objetivos. | 7.26 | -23.14 · 0.73 · 24.68 | Evaluada (notebooks 3.2 y 3.3); no entró al modelo |
| `fantasy_points_ppr` | Puntos de fantasy en formato PPR (1 punto por recepción). Se comprobó que equivalen a los puntos estándar más 1 por recepción en 100 % de las filas WR. | 18 | -3 · 5.2 · 57.9 | Insumo: entra al modelo como `fantasy_points_ppr_last5_avg`, `fantasy_points_ppr_career_avg`, `fantasy_points_ppr_last3_avg` |
| `fantasy_points` | Puntos de fantasy estándar, sumando recepción, acarreo y retornos: 0.1 por yarda, 6 por TD, −2 por fumble perdido, 2 por conversión de 2 puntos (esa regla reproduce el valor en 99.53 % de las filas WR; el resto no se investigó). | 11 | -3 · 2.7 · 44.9 | Fuente |
| `receiving_yards_after_catch` | Yardas ganadas tras la recepción (YAC). La fuente avisa que es una estadística no oficial y puede variar levemente entre fuentes. | 9 | -23 · 6 · 153 | Insumo: entra al modelo como `receiving_yards_after_catch_career_avg`, `receiving_yards_after_catch_season_avg` |
| `receiving_fumbles` | Balones que soltó tras una recepción. | 0 | 0 · 0 · 2 | Fuente |
| `receiving_fumbles_lost` | De esos, los que el rival recuperó. | 0 | 0 · 0 · 2 | Fuente |
| `receiving_2pt_conversions` | Conversiones de 2 puntos por recepción. | 0 | 0 · 0 · 1 | Evaluada (notebooks 3.2 y 3.3); no entró al modelo |
| `racr` | *Receiving Air Conversion Ratio*: yardas ganadas entre yardas aéreas. Mide cuánto convierte en yardas lo que le lanzan: >1 gana más de lo que viajó el balón (mucha YAC), <1 menos. Se comprobó como `receiving_yards` ÷ `receiving_air_yards` (coincide en 100 % de las filas con valor); queda vacío cuando no hay yardas aéreas que dividir (casi siempre, semanas sin objetivos). | 0.643 = 110 ÷ 171 | -2.00 · 0.74 · 116.00 | Evaluada (notebooks 3.1 y 3.2); ver `racr_acotado` en la sección 9 |
| `special_teams_tds` | Touchdowns en retornos de patada o despeje. | 0 | 0 · 0 · 1 | Fuente |
| `carries`, `rushing_yards` | Acarreos y yardas corriendo. Un receptor casi nunca corre con el balón (salvo jugadas de «barrida»). | 0 y 0 | 0 · 0 · 19 | Fuente (más columnas de acarreo en el Apéndice A.2) |

## 7. Variables que construye `features.py`

### 7.1 Ratios de eficiencia

Dividen una estadística entre otra para medir **calidad** y no solo volumen. Un denominador en cero (partido sin objetivos) deja el valor vacío, no cero ni infinito: sin objetivos no hay tasa de acierto que calcular.

| Variable | Cómo se calcula | Ejemplo (Ja'Marr Chase, s. 13) | Dónde se usa |
|---|---|---|---|
| `catch_rate` | `receptions` ÷ `targets`: de los pases que le lanzan, qué fracción atrapa. | 7 ÷ 14 = 0.500 | Evaluada (notebooks 3.2 y 3.3); no entró al modelo |
| `yards_per_target` | `receiving_yards` ÷ `targets`: yardas que genera por cada pase que recibe. | 110 ÷ 14 = 7.857 | Evaluada (notebooks 3.2 y 3.3); no entró al modelo |
| `air_yards_per_target` | `receiving_air_yards` ÷ `targets`: profundidad promedio con la que lo buscan. | 171 ÷ 14 = 12.214 | Evaluada (notebooks 3.2 y 3.3); no entró al modelo |

Caso sin objetivos: DeAndre Carter (CLE, semana 1 de 2025) tuvo 0 objetivos, así que `catch_rate`, `yards_per_target` y `racr` quedan vacíos en esa fila.

### 7.2 Historial del jugador: cuatro ventanas

Es la idea central del modelo: **describir al jugador con lo que hizo antes de ese partido**. Para cada una de las 10 estadísticas base (`COLUMNAS_BASE_MODELADO`: las 9 de las que salen las variables del modelo, más `receiving_tds`, cuyos promedios usa la referencia de los notebooks) se calculan cuatro promedios, nombrados `{estadística}_{ventana}_avg`:

| Sufijo | Qué promedia | Pregunta que responde |
|---|---|---|
| `_last3_avg` | Sus últimas 3 apariciones | ¿Cómo viene jugando ahora mismo? |
| `_last5_avg` | Sus últimas 5 apariciones | Forma reciente, algo más estable |
| `_season_avg` | Todos sus partidos de la temporada en curso, hasta antes del actual (se reinicia cada temporada) | ¿Cuál es su nivel este año? |
| `_career_avg` | Toda su carrera hasta antes del actual | ¿Cuál es su nivel histórico? |

**Ejemplo con Ja'Marr Chase, semana 13.** Sus apariciones previas más recientes fueron las semanas 5, 6, 7, 8, 9, 11 de 2025. Entre la 9 y la 13 faltan dos semanas que no cuentan como aparición: la semana 10 fue el descanso de su equipo, y en la 12 su equipo jugó pero él no tiene fila en la tabla de estadísticas. **«Últimas 3» son sus últimas 3 apariciones reales, no 3 semanas de calendario.**

| Aparición previa | `receptions` | `targets` | `receiving_yards` |
|---|---|---|---|
| Semana 6 | 10 | 12 | 94 |
| Semana 7 | 16 | 23 | 161 |
| Semana 8 | 12 | 19 | 91 |
| Semana 9 | 6 | 8 | 111 |
| Semana 11 | 3 | 10 | 30 |

Con eso, calculados a mano y comprobados contra la tabla (coinciden las 3 × 4 cifras): `_last3_avg` usa las semanas 8, 9, 11; `_last5_avg`, las semanas 6, 7, 8, 9, 11; `_season_avg`, las 10 apariciones de 2025 antes de la 13; `_career_avg`, las 72 apariciones de su carrera.

| Estadística | `_last3_avg` | `_last5_avg` | `_season_avg` | `_career_avg` |
|---|---|---|---|---|
| `receptions` | 7.00 | 9.40 | 7.90 | 6.58 |
| `targets` | 12.33 | 14.40 | 11.70 | 9.71 |
| `receiving_yards` | 77.33 | 97.40 | 86.10 | 87.31 |

**Reglas del cálculo** (`agregar_promedios_jugador`):

- Siempre se desplaza una fila antes de promediar (`shift(1)`): el partido actual nunca entra a su propio promedio. Esto se verificó fila por fila en el notebook 3.1.
- En la primera aparición de un jugador no hay historia, así que sus ventanas quedan vacías; con una sola aparición previa, la ventana de 3 es ese único valor. Los modelos XGBoost aceptan valores vacíos tal cual.
- Las ventanas cuentan apariciones, no semanas de calendario (el ejemplo de arriba).

### 7.3 Perfil del jugador

| Variable | Qué es y cómo se calcula | Ejemplo (Ja'Marr Chase, s. 13) | Rango observado | Dónde se usa |
|---|---|---|---|---|
| `edad` | Años del jugador al 1 de septiembre de esa temporada: (1-sep-2025 − fecha de nacimiento) ÷ 365.25. Fecha de nacimiento: `birth_date` de `load_players()`. | nació el 2000-03-01 → 25.50 años | 20.8 · 25.5 · 38.0 | **Modelo** |
| `anios_experiencia` | Temporada del partido menos temporada de novato (`rookie_season`). Un novato tiene 0. | 2025 − 2021 = 4 | 0 · 3 · 16 | Solo en los modelos lineales de los notebooks, junto con sus versiones cuadrática y por tramos; no entra a los modelos de árboles |
| `anios_experiencia_sq` | El cuadrado de la experiencia. La relación real tiene un pico entre los 3 y 5 años y luego baja; un modelo lineal solo la captura con un término cuadrático. | 4² = 16 | 0 · 9 · 256 | Solo para Ridge (modelo lineal); los modelos vigentes son de árboles y no la necesitan |
| `anios_experiencia_bucket` | Experiencia en 4 grupos: Rookie (0), 1-2 años, 3-5 años, 6+ años. Mismo propósito que la anterior. | 3-5 años | 3-5 años: 7,494 · 1-2 años: 7,261 · 6+ años: 5,068 · Rookie: 3,787 | Solo para Ridge |
| `draft_pick` | Número de selección en el draft de la NFL (1 = primer elegido). Más bajo = elegido antes. Los no drafteados reciben **el peor pick real + 1** (no la media), porque por definición son peores que el último elegido. | Ja'Marr Chase: pick 5 (ronda 1, CIN) | 3 · 117 · 473 | **Modelo** |
| `depth_team` | Lugar en la alineación de su equipo esa semana (sección 1). Más bajo = titular. | 1 (titular) | 1 · 2 · 8; vacío en 7.5 % de las filas | **Modelo** |

Ejemplo de `draft_pick` imputado: Kendrick Bourne (SF) no fue drafteado, así que su valor en la tabla es 473 (el mayor pick real es 472, más 1). Entre los 700 receptores de la tabla, 40.3 % no fue drafteado.

`years_of_experience` de `load_players()` no se usa: es un único valor por jugador (la tabla tiene una fila por jugador, no por temporada) y para Ja'Marr Chase marca 6, mientras que su experiencia en 2025 fue 4.

## 8. Las 25 variables que usan los modelos

Son las **mismas 25, en el mismo orden, para los tres modelos** (`receptions`, `receiving_yards` y `receiving_tds`, todos XGBoost; el de touchdowns usa objetivo Poisson). Salieron del notebook 3.3: un orden estable de las 110 candidatas por eliminación recursiva y el conjunto más chico que no es más de 1% peor que el mejor en validación ([ADR 0005](../decisions/0005-seleccion-de-variables.md)). Se guardan junto a cada modelo (`weekly/wr/models/*_metadata.json`), que es lo que `modeling.py` lee para ordenarlas igual que en el entrenamiento.

**Peso**: parte de la mejora total del modelo que se atribuye a esa variable (importancia por ganancia de XGBoost; cada modelo suma 100 %, salvo redondeo). Sirve para ver qué usa cada modelo, no para afirmar causalidad. «—» = el modelo no la usa en ningún corte. Los tres modelos usan las 25 en al menos un corte, pero con pesos muy distintos entre variables.

| Variable | Qué es | Ejemplo (s. 13) | Rango (mín · mediana · máx) | Peso recepciones | Peso yardas | Peso TDs |
|---|---|---|---|---|---|---|
| `receptions_last3_avg` | Recepciones: promedio de sus últimas 3 apariciones | 7.00 | 0.00 · 2.33 · 12.67 | 5.2 % | 1.4 % | 0.9 % |
| `receptions_last5_avg` | Recepciones: promedio de sus últimas 5 apariciones | 9.40 | 0.00 · 2.40 · 12.50 | 20.7 % | 0.2 % | 4.0 % |
| `receptions_season_avg` | Recepciones: promedio de esta temporada hasta antes del partido | 7.90 | 0.00 · 2.44 · 16.00 | 4.2 % | 3.3 % | 0.9 % |
| `receptions_career_avg` | Recepciones: promedio de toda su carrera hasta antes del partido | 6.58 | 0.00 · 2.47 · 12.50 | 4.2 % | 0.3 % | 0.4 % |
| `targets_last5_avg` | Objetivos (pases dirigidos a él): promedio de sus últimas 5 apariciones | 14.40 | 0.00 · 4.00 · 17.50 | 20.7 % | 2.4 % | 2.5 % |
| `targets_season_avg` | Objetivos (pases dirigidos a él): promedio de esta temporada hasta antes del partido | 11.70 | 0.00 · 4.00 · 21.00 | 1.4 % | 1.6 % | 1.6 % |
| `receiving_yards_last5_avg` | Yardas recibidas: promedio de sus últimas 5 apariciones | 97.40 | -3.50 · 30.40 · 180.00 | 0.1 % | 4.9 % | 5.8 % |
| `receiving_yards_career_avg` | Yardas recibidas: promedio de toda su carrera hasta antes del partido | 87.31 | -3.50 · 32.50 · 180.00 | 0.3 % | 4.9 % | 7.3 % |
| `receiving_yards_after_catch_season_avg` | Yardas tras la recepción: promedio de esta temporada hasta antes del partido | 42.10 | -4.00 · 9.00 · 134.00 | 0.6 % | 0.4 % | 1.2 % |
| `receiving_yards_after_catch_career_avg` | Yardas tras la recepción: promedio de toda su carrera hasta antes del partido | 39.18 | -3.00 · 10.29 · 92.00 | 1.0 % | 0.5 % | 0.9 % |
| `receiving_first_downs_career_avg` | Primeros downs por recepción: promedio de toda su carrera hasta antes del partido | 4.10 | 0.00 · 1.50 · 8.00 | 0.8 % | 0.6 % | 4.5 % |
| `target_share_last3_avg` | Participación en los objetivos del equipo: promedio de sus últimas 3 apariciones | 0.337 | 0.00 · 0.12 · 0.48 | 1.7 % | 2.3 % | 1.6 % |
| `target_share_last5_avg` | Participación en los objetivos del equipo: promedio de sus últimas 5 apariciones | 0.364 | 0.00 · 0.13 · 0.48 | 26.7 % | 47.3 % | 13.0 % |
| `target_share_season_avg` | Participación en los objetivos del equipo: promedio de esta temporada hasta antes del partido | 0.325 | 0.00 · 0.12 · 0.59 | 0.6 % | 0.9 % | 3.0 % |
| `air_yards_share_last3_avg` | Participación en las yardas aéreas del equipo: promedio de sus últimas 3 apariciones | 0.351 | -0.08 · 0.16 · 0.97 | 0.2 % | 0.4 % | 2.4 % |
| `wopr_last3_avg` | Índice de oportunidad (wopr): promedio de sus últimas 3 apariciones | 0.751 | 0.00 · 0.30 · 1.24 | 0.3 % | 2.9 % | 12.2 % |
| `wopr_season_avg` | Índice de oportunidad (wopr): promedio de esta temporada hasta antes del partido | 0.748 | 0.00 · 0.30 · 1.52 | 0.4 % | 1.8 % | 2.0 % |
| `fantasy_points_ppr_last3_avg` | Puntos de fantasy PPR: promedio de sus últimas 3 apariciones | 14.73 | -2.78 · 6.50 · 39.33 | 1.3 % | 2.1 % | 2.5 % |
| `fantasy_points_ppr_last5_avg` | Puntos de fantasy PPR: promedio de sus últimas 5 apariciones | 21.48 | -2.78 · 6.66 · 36.00 | 5.8 % | 14.9 % | 25.0 % |
| `fantasy_points_ppr_career_avg` | Puntos de fantasy PPR: promedio de toda su carrera hasta antes del partido | 19.53 | -2.78 · 7.08 · 36.00 | 1.1 % | 2.0 % | 1.5 % |
| `target_share_x_ofensiva_equipo` | Participación reciente (`target_share_last5_avg`) por yardas de recepción de los WR de su equipo en la temporada (`equipo_receiving_yards_season_avg`) | 60.62 | 0.00 · 18.08 · 130.87 | 0.2 % | 1.3 % | 2.1 % |
| `total_implicito_equipo` | Puntos que las líneas de apuestas esperan que anote su equipo: (total + línea) / 2 si es local, (total − línea) / 2 si es visitante | 22.75 | 9.75 · 22.50 · 36.25 | 0.3 % | 0.5 % | 1.3 % |
| `depth_team` | Lugar en la alineación de su equipo (1 = titular) | 1 | 1.00 · 2.00 · 8.00; vacío en 7.5 % de las filas | 2.0 % | 2.4 % | 1.7 % |
| `draft_pick` | Número de selección en el draft (más bajo = elegido antes) | 5 | 3.00 · 117.00 · 473.00 | 0.2 % | 0.3 % | 1.2 % |
| `edad` | Edad al 1 de septiembre de la temporada | 25.50 | 20.77 · 25.53 · 37.98 | 0.2 % | 0.4 % | 0.5 % |

**Las 5 variables con más peso en cada modelo:**

| Puesto | `receptions` | `receiving_yards` | `receiving_tds` |
|---|---|---|---|
| 1 | `target_share_last5_avg` (26.7 %) | `target_share_last5_avg` (47.3 %) | `fantasy_points_ppr_last5_avg` (25.0 %) |
| 2 | `receptions_last5_avg` (20.7 %) | `fantasy_points_ppr_last5_avg` (14.9 %) | `target_share_last5_avg` (13.0 %) |
| 3 | `targets_last5_avg` (20.7 %) | `receiving_yards_career_avg` (4.9 %) | `wopr_last3_avg` (12.2 %) |
| 4 | `fantasy_points_ppr_last5_avg` (5.8 %) | `receiving_yards_last5_avg` (4.9 %) | `receiving_yards_career_avg` (7.3 %) |
| 5 | `receptions_last3_avg` (5.2 %) | `receptions_season_avg` (3.3 %) | `receiving_yards_last5_avg` (5.8 %) |

En los tres modelos pesa sobre todo la participación reciente: `target_share_last5_avg` encabeza recepciones (26.7 %), `target_share_last5_avg` encabeza yardas (47.3 %) y `fantasy_points_ppr_last5_avg` encabeza touchdowns (25.0 %).

Las 25 variables no son 25 ideas distintas: 20 salen de 9 estadísticas base en distintas ventanas, y el resto son el perfil del jugador (`depth_team`, `draft_pick`, `edad`), el total implícito y una interacción. Varias miden lo mismo con ventanas distintas y están muy correlacionadas entre sí, lo que no perjudica a los modelos de árboles pero hace que sea poco confiable leer el peso de una sola variable.

## 9. Variables construidas y descartadas

Se construyeron en la Fase 4, se evaluaron en los notebooks 3.1 a 3.3 y **no entran al modelo**. Viven en `features_exploratorio.py` y solo las usan los notebooks. La razón de cada descarte está en la tabla de decisiones del encabezado de `features.py`.

| Variable | Qué es | Ejemplo (Ja'Marr Chase, s. 13) | Decisión y evidencia |
|---|---|---|---|
| `racr_acotado` | `racr` topado en su percentil 99 (5.5), para que un valor disparado no arrastre el promedio. El máximo real de `racr` en la tabla es 116. | Khalil Shakir (semana 9): 43 yardas con 1 yarda aérea → `racr` 43.0, acotado a 5.5. Chase: 0.643 (sin cambio). | Fuera de las 25 primeras del orden estable (notebook 3.3). |
| `receiving_yards_volatilidad5`, `targets_volatilidad5` | Desviación estándar de las últimas 5 apariciones: qué tan parejo o irregular es el jugador (alta = «boom-bust»). | yardas: 47.0; objetivos: 6.3 | Fuera de las 25 primeras del orden estable (notebook 3.3). |
| `equipo_receiving_yards_season_avg`, `_last3_avg`, `_last5_avg`, `_career_avg` | Ofensiva de equipo: yardas por recepción sumadas de todos los WR del equipo cada partido, promediadas con las mismas ventanas y sin incluir el partido actual. | CIN: 166.5 (temporada), 186.3 (últimos 3), 200 (últimos 5) | No entran solas (fuera de las 25 del notebook 3.3); `equipo_receiving_yards_season_avg` es insumo de `target_share_x_ofensiva_equipo`, que sí entra (sección 8). |
| `cambio_qb_titular` | `True` si el QB titular del equipo (el pasador con más intentos ese partido) es distinto al de la aparición anterior del jugador. | Joe Burrow en la semana 13; en la 11 fue Joe Flacco → `True` | Fuera de las 25 (lugar 109 de 110 en el orden estable del notebook 3.3). Además no se conoce antes del partido: el QB titular se identifica con los pases de esa misma semana. |
| `cambio_equipo` | `True` si el jugador cambió de equipo respecto a su aparición anterior (un traspaso también cambia al QB). | `False` (CIN en ambas) | Fuera de las 25 primeras del orden estable (notebook 3.3). |
| `draft_pick_x_experiencia` | Producto de `draft_pick` × `anios_experiencia`. | 5 × 4 = 20 | Fuera de las 25 primeras del orden estable (notebook 3.3). |
| `cambio_qb_x_target_share` | Producto de `cambio_qb_titular` (1 o 0) × `target_share_last5_avg`. | 1 × 0.364 = 0.364 | Fuera de las 25 (lugar 92); depende del cambio de QB, que no se conoce antes del partido. |

### Contexto del partido (calendario)

Columnas de `load_schedules()` que se probaron como contexto. *rest*, *div_game*, *roof* y *surface* se evaluaron en los notebooks 3.2 y 3.3 y no entraron, y el clima no mostró efecto en el análisis exploratorio. Las líneas de apuestas sí entran, combinadas en el total implícito del equipo (sección 8).

| Variable | Qué es | Ejemplo (este partido) |
|---|---|---|
| `away_rest` / `home_rest` | Días de descanso que trae cada equipo antes del partido. | 4 y 4 (partido en jueves de Acción de Gracias) |
| `div_game` | 1 si los dos equipos son de la misma división. | 1 |
| `spread_line` | Línea de apuestas: positiva = favorito el local por esa cantidad de puntos. | 7.0 (BAL favorito por 7) |
| `total_line` | Puntos totales que las apuestas esperaban entre ambos equipos. | 52.5; el total real fue 46 |
| `roof`, `surface` | Techo del estadio (`outdoors`, `open`, `closed`, `dome`) y tipo de superficie. | outdoors, grass |
| `temp`, `wind` | Temperatura (°F) y viento (mph); solo para estadios abiertos. | 38 °F, 11 mph |

También se evaluaron en la Fase 3 y no tienen columna en el pipeline: la dureza defensiva del rival contra WR y las jugadas de carrera de un receptor. Ambas se descartaron por no mostrar efecto.

## 10. Salida del pipeline

`pipeline.py` escribe en `weekly/wr/outputs/{temporada}/`, según el estado de la semana
([ADR 0006](../decisions/0006-seguimiento-semanal.md)):

- **Semana pendiente:** `predicciones_semana_N.csv`, la predicción emitida, y `variables_semana_N.csv`, las 25 variables de la sección 8 con que se hizo, una fila por receptor de la alineación.
- **Semana jugada:** `evaluacion_semana_N.csv`, predicción y valor real por receptor, y `metricas_semana_N.csv`, las métricas de la semana.

Cada corrida agrega además sus filas al historial, `weekly/wr/outputs/tracking.csv` (sección 10.3).

### 10.1 Predicción y evaluación

| Columna | Qué es | Ejemplo (Chase, semana 3 de 2026) |
|---|---|---|
| `receptions_pred` | Recepciones esperadas. | 6.236 |
| `receiving_yards_pred` | Yardas esperadas. | 76.745 |
| `receiving_tds_pred` | Touchdowns esperados: un promedio, no un 0 o un 1. Un valor de 0.3 equivale a un TD cada tres partidos con este perfil. | 0.485 |
| `receptions_real`, `receiving_yards_real`, `receiving_tds_real` | Lo que de verdad pasó. Solo en la evaluación. | 9, 98, 1 |
| `origen_prediccion` | `emitida` si es la predicción guardada antes de los partidos; `reconstruida` si no existía y se recalculó con los modelos vigentes (las semanas 1-4 de 2026). Solo en la evaluación. | reconstruida |

### 10.2 Métricas de la semana

Una fila por resultado (semana 3 de 2026). «—» = la métrica no aplica a ese resultado:

| Columna | Qué es | `receptions` | `receiving_yards` | `receiving_tds` |
|---|---|---|---|---|
| `n` | Cuántos WR se evaluaron. | 154 | 154 | 154 |
| `metrica_principal` | La métrica con la que se eligió el modelo: RMSE en recepciones y yardas, deviance de Poisson en touchdowns ([ADR 0004](../decisions/0004-metricas-de-seleccion.md)). | rmse | rmse | deviance_poisson |
| `rmse` | Raíz del error cuadrático medio: en las unidades del resultado, castiga más los errores grandes. Menor = mejor. | 1.793 | 27.482 | 0.402 |
| `mae` | Error absoluto medio: en promedio, cuántas unidades se equivoca la predicción. Se reporta, pero no decide: premia la mediana, no el valor esperado. | 1.308 | 19.334 | 0.280 |
| `r2` | Parte de la variación explicada, contra el promedio de la propia semana. | 0.497 | 0.396 | 0.068 |
| `r2_oos` | R² fuera de muestra: contra la media de entrenamiento guardada con el modelo. 0 = igual que predecir el promedio histórico; negativo = peor que eso. | 0.503 | 0.399 | 0.068 |
| `sesgo` | Promedio predicho menos promedio real. Positivo = el modelo se pasó en promedio. | -0.175 | -2.815 | -0.028 |
| `sesgo_media_entrenamiento` | El sesgo que tendría predecir la media de entrenamiento: refleja qué tanto cambió el nivel de esa semana respecto a la historia. | 0.269 | 2.635 | 0.007 |
| `media_real` | Promedio real de la semana. | 2.429 | 31.643 | 0.201 |
| `pendiente_calibracion` | Pendiente de lo real sobre lo predicho: 1 = predicciones en la escala correcta; menor a 1 = demasiado extremas; mayor a 1 = demasiado tímidas. | 1.137 | 1.082 | 0.847 |
| `deviance_poisson` | Deviance de Poisson: el error propio de un conteo, la métrica principal de touchdowns. Menor = mejor. No aplica a yardas. | 1.333 | — | 0.604 |
| `d2_oos` | Fracción de la deviance de Poisson explicada contra la media de entrenamiento (el equivalente de `r2_oos` para conteos). | 0.507 | — | 0.089 |
| `referencia_validacion` | Valor de la métrica principal del modelo en validación (2022–2023), guardado con el modelo, para comparar contra la semana. | 1.9315 | 29.500 | 0.613 |
| `referencia_prueba` | Lo mismo en la prueba (2024–2025). | 1.8355 | 27.935 | 0.622 |
| `brier_anota` | Solo touchdowns: error cuadrático de la probabilidad de anotar al menos uno, calculada como 1 − e^(−predicción). Menor = mejor. | — | — | 0.148 |
| `auc_anota` | Solo touchdowns: qué tan bien ordena el modelo a quienes anotan por encima de quienes no (0.5 = azar, 1 = perfecto). | — | — | 0.692 |

### 10.3 Historial de corridas (`tracking.csv`)

Una fila por corrida y resultado; las filas nunca se reescriben. Para tablas y alertas se usa la corrida más reciente de cada temporada, semana, estado y resultado. Ejemplo: la evaluación de la semana 4 de 2026 en `receiving_yards`. La huella es de la semana 5, la primera emitida.

| Columna | Qué es | Ejemplo |
|---|---|---|
| `fecha_corrida` | Fecha y hora de la corrida (UTC). | 2026-10-08T05:36:37+00:00 |
| `season`, `week` | Temporada y semana. | 2026, 4 |
| `estado` | `pendiente` (se predijo antes de los partidos) o `jugada` (se evaluó). | jugada |
| `target` | El resultado de la fila. | receiving_yards |
| `origen_prediccion` | `emitida` o `reconstruida` (sección 10.1). | reconstruida |
| `n_wr` | Receptores predichos (semana pendiente) o evaluados (semana jugada). | 151 |
| `n_predichos_sin_real` | Receptores con predicción emitida que no registraron estadísticas esa semana (por ejemplo, inactivos); no entran a las métricas. | 0 |
| `n_reales_sin_prediccion` | Receptores con estadísticas que no tenían predicción emitida (no estaban en la alineación). | 0 |
| `modelo_entrenado_con`, `modelo_fecha_guardado` | Con qué temporadas se entrenó el modelo y cuándo se guardó: identifican la versión del modelo. | 2016-2025; 2026-10-08T05:35:43 |
| `huella_variables` | Primeros 16 caracteres del SHA-256 de `variables_semana_N.csv`: con ese archivo y el modelo guardado se reproduce la predicción. Vacía si la predicción es reconstruida. | ce8183eb758cf2cd |
| `metrica_principal`, `valor` | La métrica principal y su valor en la semana (sección 10.2). | rmse, 31.800 |
| `referencia_validacion`, `referencia_prueba` | Como en la sección 10.2. | 29.500, 27.935 |
| `r2_oos`, `sesgo`, `pendiente_calibracion` | Como en la sección 10.2, de la semana. | 0.394, -5.379, 1.277 |
| `ventana` | Las cuatro últimas semanas jugadas de la temporada; vacía hasta la semana 4. | 1-4 |
| `ventana_valor`, `ventana_sesgo`, `ventana_pendiente` | Métrica principal, sesgo y pendiente de las cuatro semanas juntas. | 29.596, -3.074, 1.105 |
| `alertas` | Métricas de la ventana fuera de sus límites de control, con el límite. «(persistente)» si también lo estaba la ventana que termina cuatro semanas antes. | sesgo -3.074 < -1.84 |
| `advertencias` | Chequeos de calidad que fallaron sin detener la corrida. | valores dentro del rango historico: air_yards_share_last3_avg (1 filas); prediccion emitida encontrada: se recalculo con los modelos vigentes |

## Apéndice A. Resto de columnas de la tabla de estadísticas

La tabla de `load_player_stats()` trae 150 columnas para todas las posiciones. Las de recepción están en las secciones 4 a 6. Aquí van las demás, que sirven a otras posiciones (pase, acarreo, defensa, patadas) y que el pipeline de WR no usa. Definiciones: diccionario oficial de `nflverse`, traducidas. Marcadas con (*) las columnas que el diccionario no define; su significado se dedujo del nombre y, para los conteos por distancia, se verificó contra las jugadas de 2024.

**Ejemplo:** para cada columna se muestra el valor de mayor magnitud que registró alguien en el mismo partido (2025_13_CIN_BAL, 68 filas de jugadores), con el nombre del jugador. «Nadie» significa que ninguno la registró en este partido.

### A.1 Pase (QB)

| Variable | Qué mide | Ejemplo del partido |
|---|---|---|
| `completions` | Pases completos. | Joe Burrow: 24 |
| `attempts` | Pases intentados. | Joe Burrow: 46 |
| `passing_yards` | Yardas ganadas por pase. | Joe Burrow: 261 |
| `passing_tds` | Touchdowns de pase. | Joe Burrow: 2 |
| `passing_interceptions` | Pases interceptados por la defensa. | Lamar Jackson: 1 |
| `sacks_suffered` | Capturas (*sacks*) recibidas como QB. | Lamar Jackson: 3 |
| `sack_yards_lost` | Yardas perdidas por esas capturas. | Lamar Jackson: -23 |
| `sack_fumbles` | Capturas en las que el QB soltó el balón. | Lamar Jackson: 2 |
| `sack_fumbles_lost` | De esas, las que el rival recuperó. | Lamar Jackson: 2 |
| `passing_air_yards` | Yardas aéreas de sus pases, incluidos los incompletos. | Lamar Jackson: 381 |
| `passing_yards_after_catch` | Yardas que ganaron sus receptores tras atrapar (estadística no oficial). | Lamar Jackson: 119 |
| `passing_first_downs` | Primeros downs por pase. | Joe Burrow: 11 |
| `passing_epa` | EPA total en pases y capturas (acredita al QB hasta el momento en que un receptor pierde un balón tras atrapar). | Lamar Jackson: -11.81 |
| `passing_cpoe` | Porcentaje de pases completos por encima de lo esperado. | Lamar Jackson: -6.80 |
| `passing_2pt_conversions` | Conversiones de 2 puntos por pase. | Nadie |
| `pacr` | Yardas de pase entre yardas aéreas lanzadas, por partido (el equivalente de `racr` para el QB). | Joe Burrow: 0.69 |
| `passing_10` | Pases completos de 10 o más yardas (*). | Joe Burrow: 10 |
| `passing_16` | Pases completos de 16 o más yardas (*). | Lamar Jackson: 5 |
| `passing_20` | Pases completos de 20 o más yardas (*). | Lamar Jackson: 3 |
| `passing_40` | Pases completos de 40 o más yardas (*). | Lamar Jackson: 2 |

### A.2 Acarreo

| Variable | Qué mide | Ejemplo del partido |
|---|---|---|
| `carries` | Acarreos oficiales (incluye escapadas del QB y rodillas). | Chase Brown: 15 |
| `rushing_yards` | Yardas corriendo. | Chase Brown: 78 |
| `rushing_tds` | Touchdowns corriendo. | Derrick Henry: 1 |
| `rushing_fumbles` | Acarreos con balón suelto. | Samaje Perine: 1 |
| `rushing_fumbles_lost` | De esos, los recuperados por el rival. | Samaje Perine: 1 |
| `rushing_first_downs` | Primeros downs corriendo. | Chase Brown: 4 |
| `rushing_epa` | EPA en acarreos. | Samaje Perine: -7.19 |
| `rushing_2pt_conversions` | Conversiones de 2 puntos corriendo. | Nadie |
| `rushing_10` | Acarreos de 10 o más yardas (*). | Chase Brown: 3 |
| `rushing_12` | Acarreos de 12 o más yardas (*). | Chase Brown: 2 |
| `rushing_20` | Acarreos de 20 o más yardas (*). | Derrick Henry: 1 |
| `rushing_40` | Acarreos de 40 o más yardas (*). | Nadie |

### A.3 Defensa

| Variable | Qué mide | Ejemplo del partido |
|---|---|---|
| `def_tackles_solo` | Tacleadas individuales. | Marlon Humphrey: 7 |
| `def_tackles_with_assist` | Tacleadas hechas con ayuda de un compañero. | John Jenkins: 1 |
| `def_tackle_assists` | Tacleadas de asistencia (ayudó a un compañero). | Roquan Smith: 8 |
| `def_tackles_for_loss` | Tacleadas con pérdida de yardas para el rival. | Joseph Ossai: 2 |
| `def_tackles_for_loss_yards` | Yardas perdidas por el rival en esas tacleadas. | Joseph Ossai: 18 |
| `def_fumbles_forced` | Balones sueltos del rival que provocó (la fuente lo redacta al revés, pero en el partido de ejemplo suma 3 para CIN y 1 para BAL, los balones sueltos de cada rival). | Jalen Davis: 1 |
| `def_sacks` | Capturas al QB. | Joseph Ossai: 2 |
| `def_sack_yards` | Yardas perdidas por el rival en sus capturas. | Joseph Ossai: 18 |
| `def_qb_hits` | Golpes al QB sin captura. | Joseph Ossai: 4 |
| `def_interceptions` | Intercepciones. | Demetrius Knight Jr.: 1 |
| `def_interception_yards` | Yardas ganadas o perdidas al devolver intercepciones. | Demetrius Knight Jr.: 39 |
| `def_pass_defended` | Pases defendidos o desviados. | Chidobe Awuzie: 2 |
| `def_tds` | Touchdowns anotados por la defensa. | Nadie |
| `def_fumbles` | Balones sueltos del propio jugador. | Nadie |
| `def_safeties` | Safeties provocados. | Nadie |
| `def_punt_blocks` | Despejes bloqueados (*). | Nadie |
| `def_pat_blocks` | Puntos extra bloqueados (*). | Nadie |
| `def_fg_blocks` | Goles de campo bloqueados (*). | Nadie |
| `def_2pt_atts` | Intentos de conversión de 2 puntos defendidos (*). | Nadie |
| `def_2pt_made` | Conversiones de 2 puntos que permitió (*). | Nadie |

### A.4 Balones sueltos, castigos y miscelánea

| Variable | Qué mide | Ejemplo del partido |
|---|---|---|
| `misc_yards` | Yardas misceláneas atribuidas al jugador. | Nadie |
| `fumble_recovery_own` | Balones sueltos de su propio equipo que recuperó. | Nadie |
| `fumble_recovery_yards_own` | Yardas en esas recuperaciones. | Nadie |
| `fumble_recovery_opp` | Balones sueltos del rival que recuperó. | Cedric Johnson: 2 |
| `fumble_recovery_yards_opp` | Yardas en esas recuperaciones. | DJ Turner II: 7 |
| `fumble_recovery_tds` | Recuperaciones de balón suelto llevadas a touchdown. | Nadie |
| `penalties` | Castigos atribuidos al jugador. | Orlando Brown: 2 |
| `penalty_yards` | Yardas de castigo. | T.J. Tampa: 17 |
| `fumbles_forced_by_opp` | Balones sueltos por el jugador provocados por el rival (*). | Samaje Perine: 1 |
| `fumbles_not_forced` | Balones sueltos sin provocación del rival (*). | Lamar Jackson: 1 |
| `fumbles_out_of_bounds` | Balones sueltos que salieron del campo (*). | Isaiah Likely: 1 |
| `fumbles_total` | Total de balones sueltos del jugador (*). | Lamar Jackson: 2 |
| `fumbles_lost_total` | Total de balones perdidos ante el rival (*). | Lamar Jackson: 2 |

### A.5 Retornos

| Variable | Qué mide | Ejemplo del partido |
|---|---|---|
| `punt_returns` | Retornos de despeje. | Charlie Jones: 1 |
| `punt_return_yards` | Yardas en retornos de despeje. | Mitchell Tinsley: 10 |
| `kickoff_returns` | Retornos de patada inicial. | Gary Brightwell: 3 |
| `kickoff_return_yards` | Yardas en retornos de patada inicial. | Keaton Mitchell: 91 |

### A.6 Pateador: goles de campo y puntos extra

| Variable | Qué mide | Ejemplo del partido |
|---|---|---|
| `fg_made` | Goles de campo anotados. | Evan McPherson: 6 |
| `fg_att` | Goles de campo intentados. | Evan McPherson: 6 |
| `fg_missed` | Goles de campo fallados. | Nadie |
| `fg_blocked` | Goles de campo bloqueados. | Nadie |
| `fg_long` | Gol de campo anotado más largo (yardas). | Evan McPherson: 52 |
| `fg_pct` | Proporción de goles de campo anotados (1 = todos). | Evan McPherson: 1 |
| `fg_made_0_19` | Goles anotados de 0 a 19 yardas. | Nadie |
| `fg_made_20_29` | Goles anotados de 20 a 29 yardas. | Evan McPherson: 1 |
| `fg_made_30_39` | Goles anotados de 30 a 39 yardas. | Evan McPherson: 2 |
| `fg_made_40_49` | Goles anotados de 40 a 49 yardas. | Evan McPherson: 2 |
| `fg_made_50_59` | Goles anotados de 50 a 59 yardas. | Evan McPherson: 1 |
| `fg_made_60_` | Goles anotados de más de 60 yardas. | Nadie |
| `fg_missed_0_19` | Goles fallados de 0 a 19 yardas. | Nadie |
| `fg_missed_20_29` | Goles fallados de 20 a 29 yardas. | Nadie |
| `fg_missed_30_39` | Goles fallados de 30 a 39 yardas. | Nadie |
| `fg_missed_40_49` | Goles fallados de 40 a 49 yardas. | Nadie |
| `fg_missed_50_59` | Goles fallados de 50 a 59 yardas. | Nadie |
| `fg_missed_60_` | Goles fallados de más de 60 yardas. | Nadie |
| `fg_made_list` | Distancias de los goles anotados, en un solo texto separado por «;» (la fuente dice comas, pero los datos traen «;»). | Evan McPherson: 31;42;24;33;52;41 |
| `fg_missed_list` | Distancias de los goles fallados, en un solo texto separado por «;». | Nadie |
| `fg_blocked_list` | Distancias de los goles bloqueados, en un solo texto separado por «;». | Nadie |
| `fg_made_distance` | Suma de distancias de los goles anotados. | Evan McPherson: 223 |
| `fg_missed_distance` | Suma de distancias de los goles fallados. | Nadie |
| `fg_blocked_distance` | Suma de distancias de los goles bloqueados. | Nadie |
| `pat_made` | Puntos extra anotados. | Evan McPherson: 2 |
| `pat_att` | Puntos extra intentados. | Evan McPherson: 2 |
| `pat_missed` | Puntos extra fallados. | Nadie |
| `pat_blocked` | Puntos extra bloqueados. | Nadie |
| `pat_pct` | Proporción de puntos extra anotados (1 = todos). | Evan McPherson: 1 |
| `gwfg_made` | Goles de campo de la victoria anotados. | Nadie |
| `gwfg_att` | Goles de campo de la victoria intentados. | Nadie |
| `gwfg_missed` | Goles de la victoria fallados. | Nadie |
| `gwfg_blocked` | Goles de la victoria bloqueados. | Nadie |
| `gwfg_distance` | Distancia total de goles de la victoria anotados. | Nadie |

### A.7 Despeje (*punter*)

| Variable | Qué mide | Ejemplo del partido |
|---|---|---|
| `pt_att` | Despejes (*punts*) realizados (*). | Jordan Stout: 3 |
| `pt_blocked` | Despejes bloqueados (*). | Nadie |
| `pt_long` | Despeje más largo (*). | Jordan Stout: 61 |
| `pt_yards` | Yardas totales de despeje (*). | Jordan Stout: 149 |
| `pt_inside_20` | Despejes que dejaron al rival dentro de su yarda 20 (*). | Ryan Rehkow: 1 |
| `pt_out_of_bounds` | Despejes que salieron del campo (*). | Nadie |
| `pt_downed` | Despejes detenidos por el equipo despejador (*). | Ryan Rehkow: 1 |
| `pt_touchback` | Despejes que terminaron en *touchback* (*). | Jordan Stout: 1 |
| `pt_fair_caught` | Despejes recibidos con captura limpia (*). | Nadie |
| `pt_returned` | Despejes que el rival devolvió (*). | Jordan Stout: 2 |
| `pt_return_yards` | Yardas devueltas por el rival sobre sus despejes (*). | Jordan Stout: 19 |
| `pt_return_tds` | Touchdowns del rival en retorno de despeje (*). | Nadie |
| `pt_net_yards` | Yardas netas de despeje: brutas menos yardas devueltas y 20 por cada touchback (*). En el ejemplo: 149 − 19 − 20 = 110. | Jordan Stout: 110 |

## Apéndice B. Columnas de las tablas de jugadores y calendario

### B.1 Jugadores (`load_players`)

| Columna | Qué es | Ejemplo (Ja'Marr Chase) | ¿La usa el pipeline? |
|---|---|---|---|
| `gsis_id` | Identificador único del jugador; es la llave del proyecto (en la tabla de estadísticas se llama `player_id`). | 00-0036900 | sí |
| `display_name` | Nombre completo. | Ja'Marr Chase | sí |
| `common_first_name` | Nombre de uso común. | Ja'Marr | no |
| `first_name` | Nombre. | Ja'Marr | no |
| `last_name` | Apellido. | Chase | no |
| `short_name` | Nombre corto con formato «F.Apellido». | J.Chase | no |
| `football_name` | Nombre con el que se le conoce en el juego. | Ja'Marr | no |
| `suffix` | Sufijo del nombre (Jr., II…). | vacío | no |
| `esb_id` | Identificador en ESB. | CHA694469 | no |
| `nfl_id` | Identificador en la NFL. | 53434 | no |
| `pfr_id` | Identificador en Pro-Football-Reference. | ChasJa00 | no |
| `pff_id` | Identificador en PFF. | 84270 | no |
| `otc_id` | Identificador en Over the Cap. | 9469 | no |
| `espn_id` | Identificador en ESPN. | 4362628 | no |
| `smart_id` | Identificador SMART del juego a juego (incluye un ESB_ID codificado). | 32004348-4169-4469-b021-d2c20a7d7cf5 | no |
| `birth_date` | Fecha de nacimiento (llega como texto; `data.py` la convierte a fecha). Se usa para `edad`. | 2000-03-01 | sí |
| `position_group` | Grupo de posición según la NFL. | WR | no |
| `position` | Posición según la NFL. | WR | no |
| `ngs_position_group` | Grupo de posición según Next Gen Stats. | WR | no |
| `ngs_position` | Posición según Next Gen Stats. | WR | no |
| `height` | Estatura en pulgadas. | 72 | no |
| `weight` | Peso en libras. | 205 | no |
| `headshot` | Enlace a su foto oficial. | https://static.www.nfl.com/image/upload/f_auto,q_auto/league/qya3dtjb5kgofcuj2tuw | no |
| `college_name` | Universidad (normalmente la última). | LSU | no |
| `college_conference` | Conferencia universitaria. | Southeastern Conference | no |
| `jersey_number` | Último número de camiseta. | 1 | no |
| `rookie_season` | Temporada de novato (4 dígitos). Se usa para `anios_experiencia`. | 2021 | sí |
| `last_season` | Última temporada en que estuvo activo. | 2026 | no |
| `latest_team` | Último equipo en que figuró. | CIN | no |
| `status` | Estado en la plantilla (activo, lesionado, equipo de práctica…). | ACT | no |
| `ngs_status` | Estado según Next Gen Stats. | ACT | no |
| `ngs_status_short_description` | Descripción del estado según Next Gen Stats. | Active | no |
| `years_of_experience` | Años jugados en la liga, al momento de descargar la tabla. No se usa (ver sección 7.3). | 6 | no |
| `pff_position` | Posición según PFF. | WR | no |
| `pff_status` | Estado según PFF. | A | no |
| `draft_year` | Año del draft. | 2021 | no |
| `draft_round` | Ronda del draft. | 1 | no |
| `draft_pick` | Número de selección en el draft. Se usa para `draft_pick`. | 5 | sí |
| `draft_team` | Equipo que lo seleccionó. | CIN | no |

### B.2 Calendario (`load_schedules`)

| Columna | Qué es | Ejemplo (este partido) | ¿La usa el pipeline? |
|---|---|---|---|
| `game_id` | Identificador del partido: temporada, semana, visitante, local. | 2025_13_CIN_BAL | no |
| `season` | Año de la temporada. | 2025 | sí |
| `game_type` | Tipo de partido: `REG` (temporada regular), `WC`, `DIV`, `CON`, `SB` (playoffs). `data.py` deja solo `REG`. | REG | sí (filtro) |
| `week` | Semana de la temporada. | 13 | sí |
| `gameday` | Fecha del partido (llega como texto; `data.py` la convierte a fecha). | 2025-11-27 | sí (alinea la alineación con su semana) |
| `weekday` | Día de la semana. | Thursday | no |
| `gametime` | Hora de inicio, en horario del este de EE. UU. y formato de 24 horas. | 20:20 | no |
| `away_team` | Equipo visitante. | CIN | sí |
| `away_score` | Puntos del visitante; vacío si el partido no se ha jugado. | 32 | no |
| `home_team` | Equipo local (o el designado como local si no hay local real). | BAL | sí |
| `home_score` | Puntos del local; vacío si el partido no se ha jugado. El pipeline lo usa para saber si una semana ya se jugó. | 14 | sí (¿ya se jugó?) |
| `location` | `Home` si se juega en el estadio del local, `Neutral` si es sede neutral. | Home | no |
| `result` | Puntos del local menos puntos del visitante. | -18 | no |
| `total` | Suma de los puntos de ambos equipos. | 46 | no |
| `overtime` | 1 si hubo tiempo extra (*). | 0 | no |
| `old_game_id` | Identificador antiguo de la NFL. | 2025112702 | no |
| `gsis` | Identificador del partido en el sistema GSIS de la NFL. | 60023 | no |
| `nfl_detail_id` | Identificador en NFL Detail. | vacío | no |
| `pfr` | Identificador en Pro-Football-Reference. | 202511270rav | no |
| `pff` | Identificador en PFF. | 28598 | no |
| `espn` | Identificador en ESPN. | 401772930 | no |
| `ftn` | Identificador en FTN (*). | 6914 | no |
| `away_rest` | Días de descanso del visitante. | 4 | no |
| `home_rest` | Días de descanso del local. | 4 | no |
| `away_moneyline` | Momio de que gane el visitante. | 295 | no |
| `home_moneyline` | Momio de que gane el local. | -375 | no |
| `spread_line` | Línea de apuestas: positiva = favorito el local por esos puntos. | 7 | no |
| `away_spread_odds` | Momio de que el visitante cubra la línea. | 100 | no |
| `home_spread_odds` | Momio de que el local cubra la línea. | -120 | no |
| `total_line` | Línea de puntos totales. | 52.5 | no |
| `under_odds` | Momio de que el total quede por debajo de la línea. | -115 | no |
| `over_odds` | Momio de que el total quede por encima de la línea. | -105 | no |
| `div_game` | 1 si los dos equipos son de la misma división. | 1 | no |
| `roof` | Techo del estadio: `outdoors`, `open`, `closed`, `dome`. | outdoors | no |
| `surface` | Tipo de superficie. | grass | no |
| `temp` | Temperatura; solo estadios abiertos. | 38 | no |
| `wind` | Viento en mph; solo estadios abiertos. | 11 | no |
| `away_qb_id` | Identificador del QB titular visitante. | 00-0036442 | no |
| `home_qb_id` | Identificador del QB titular local. | 00-0034796 | no |
| `away_qb_name` | QB titular visitante. | Joe Burrow | no |
| `home_qb_name` | QB titular local. | Lamar Jackson | no |
| `away_coach` | Entrenador en jefe del visitante. | Zac Taylor | no |
| `home_coach` | Entrenador en jefe del local. | John Harbaugh | no |
| `referee` | Árbitro principal. | Craig Wrolstad | no |
| `stadium_id` | Identificador del estadio. | BAL00 | no |
| `stadium` | Nombre del estadio. | M&T Bank Stadium | no |

### B.3 Alineaciones (`load_depth_charts`)

Antes de unificarse (sección 3), cada esquema trae sus propias columnas. Las que dejan los dos esquemas tras `cargar_depth_charts_unificado`:

| Columna | Qué es | Ejemplo (Chase, s. 13) |
|---|---|---|
| `season` | Temporada. En el esquema nuevo (2025+) se infiere del calendario. | 2025 |
| `week` | Semana. En el esquema nuevo se infiere asignando cada captura al próximo partido del equipo. | 13 |
| `team` | Equipo al que pertenece la alineación. | CIN |
| `player_id` | Identificador del jugador (`gsis_id` en la fuente). | 00-0036900 |
| `depth_team` | Lugar en la alineación (esquema viejo: `depth_team`; esquema nuevo: `pos_rank`). | 1 |

## Índice alfabético

Cada variable con las secciones donde se define (un nombre puede repetirse en varias tablas).

| Variable | Secciones |
|---|---|
| `advertencias` | 10.3 |
| `air_yards_per_target` | 7.1 |
| `air_yards_share` | 6 |
| `air_yards_share_last3_avg` | 8 |
| `alertas` | 10.3 |
| `anios_experiencia` | 7.3 |
| `anios_experiencia_bucket` | 7.3 |
| `anios_experiencia_sq` | 7.3 |
| `attempts` | A.1 |
| `auc_anota` | 10.2 |
| `away_coach` | B.2 |
| `away_moneyline` | B.2 |
| `away_qb_id` | B.2 |
| `away_qb_name` | B.2 |
| `away_rest` | 9, B.2 |
| `away_score` | B.2 |
| `away_spread_odds` | B.2 |
| `away_team` | B.2 |
| `birth_date` | B.1 |
| `brier_anota` | 10.2 |
| `cambio_equipo` | 9 |
| `cambio_qb_titular` | 9 |
| `cambio_qb_x_target_share` | 9 |
| `carries` | 6, A.2 |
| `catch_rate` | 7.1 |
| `college_conference` | B.1 |
| `college_name` | B.1 |
| `common_first_name` | B.1 |
| `completions` | A.1 |
| `d2_oos` | 10.2 |
| `def_2pt_atts` | A.3 |
| `def_2pt_made` | A.3 |
| `def_fg_blocks` | A.3 |
| `def_fumbles` | A.3 |
| `def_fumbles_forced` | A.3 |
| `def_interception_yards` | A.3 |
| `def_interceptions` | A.3 |
| `def_pass_defended` | A.3 |
| `def_pat_blocks` | A.3 |
| `def_punt_blocks` | A.3 |
| `def_qb_hits` | A.3 |
| `def_sack_yards` | A.3 |
| `def_sacks` | A.3 |
| `def_safeties` | A.3 |
| `def_tackle_assists` | A.3 |
| `def_tackles_for_loss` | A.3 |
| `def_tackles_for_loss_yards` | A.3 |
| `def_tackles_solo` | A.3 |
| `def_tackles_with_assist` | A.3 |
| `def_tds` | A.3 |
| `depth_team` | 7.3, 8 |
| `deviance_poisson` | 10.2 |
| `display_name` | B.1 |
| `div_game` | 9, B.2 |
| `draft_pick` | 7.3, 8, B.1 |
| `draft_pick_x_experiencia` | 9 |
| `draft_round` | B.1 |
| `draft_team` | B.1 |
| `draft_year` | B.1 |
| `edad` | 7.3, 8 |
| `equipo_receiving_yards_season_avg` | 9 |
| `es_prediccion` | 4 |
| `esb_id` | B.1 |
| `espn` | B.2 |
| `espn_id` | B.1 |
| `estado` | 10.3 |
| `fantasy_points` | 6 |
| `fantasy_points_ppr` | 6 |
| `fantasy_points_ppr_career_avg` | 8 |
| `fantasy_points_ppr_last3_avg` | 8 |
| `fantasy_points_ppr_last5_avg` | 8 |
| `fecha_corrida` | 10.3 |
| `fg_att` | A.6 |
| `fg_blocked` | A.6 |
| `fg_blocked_distance` | A.6 |
| `fg_blocked_list` | A.6 |
| `fg_long` | A.6 |
| `fg_made` | A.6 |
| `fg_made_0_19` | A.6 |
| `fg_made_20_29` | A.6 |
| `fg_made_30_39` | A.6 |
| `fg_made_40_49` | A.6 |
| `fg_made_50_59` | A.6 |
| `fg_made_60_` | A.6 |
| `fg_made_distance` | A.6 |
| `fg_made_list` | A.6 |
| `fg_missed` | A.6 |
| `fg_missed_0_19` | A.6 |
| `fg_missed_20_29` | A.6 |
| `fg_missed_30_39` | A.6 |
| `fg_missed_40_49` | A.6 |
| `fg_missed_50_59` | A.6 |
| `fg_missed_60_` | A.6 |
| `fg_missed_distance` | A.6 |
| `fg_missed_list` | A.6 |
| `fg_pct` | A.6 |
| `first_name` | B.1 |
| `football_name` | B.1 |
| `ftn` | B.2 |
| `fumble_recovery_opp` | A.4 |
| `fumble_recovery_own` | A.4 |
| `fumble_recovery_tds` | A.4 |
| `fumble_recovery_yards_opp` | A.4 |
| `fumble_recovery_yards_own` | A.4 |
| `fumbles_forced_by_opp` | A.4 |
| `fumbles_lost_total` | A.4 |
| `fumbles_not_forced` | A.4 |
| `fumbles_out_of_bounds` | A.4 |
| `fumbles_total` | A.4 |
| `game_id` | 4, B.2 |
| `game_type` | B.2 |
| `gameday` | B.2 |
| `gametime` | B.2 |
| `gsis` | B.2 |
| `gsis_id` | B.1 |
| `gwfg_att` | A.6 |
| `gwfg_blocked` | A.6 |
| `gwfg_distance` | A.6 |
| `gwfg_made` | A.6 |
| `gwfg_missed` | A.6 |
| `headshot` | B.1 |
| `headshot_url` | 4 |
| `height` | B.1 |
| `home_coach` | B.2 |
| `home_moneyline` | B.2 |
| `home_qb_id` | B.2 |
| `home_qb_name` | B.2 |
| `home_rest` | 9, B.2 |
| `home_score` | B.2 |
| `home_spread_odds` | B.2 |
| `home_team` | B.2 |
| `huella_variables` | 10.3 |
| `jersey_number` | B.1 |
| `kickoff_return_yards` | A.5 |
| `kickoff_returns` | A.5 |
| `last_name` | B.1 |
| `last_season` | B.1 |
| `latest_team` | B.1 |
| `location` | B.2 |
| `mae` | 10.2 |
| `media_real` | 10.2 |
| `metrica_principal` | 10.2, 10.3 |
| `misc_yards` | A.4 |
| `modelo_entrenado_con` | 10.3 |
| `modelo_fecha_guardado` | 10.3 |
| `n` | 10.2 |
| `n_predichos_sin_real` | 10.3 |
| `n_reales_sin_prediccion` | 10.3 |
| `n_wr` | 10.3 |
| `nfl_detail_id` | B.2 |
| `nfl_id` | B.1 |
| `ngs_position` | B.1 |
| `ngs_position_group` | B.1 |
| `ngs_status` | B.1 |
| `ngs_status_short_description` | B.1 |
| `old_game_id` | B.2 |
| `opponent_team` | 4 |
| `origen_prediccion` | 10.1, 10.3 |
| `otc_id` | B.1 |
| `over_odds` | B.2 |
| `overtime` | B.2 |
| `pacr` | A.1 |
| `passing_10` | A.1 |
| `passing_16` | A.1 |
| `passing_20` | A.1 |
| `passing_2pt_conversions` | A.1 |
| `passing_40` | A.1 |
| `passing_air_yards` | A.1 |
| `passing_cpoe` | A.1 |
| `passing_epa` | A.1 |
| `passing_first_downs` | A.1 |
| `passing_interceptions` | A.1 |
| `passing_tds` | A.1 |
| `passing_yards` | A.1 |
| `passing_yards_after_catch` | A.1 |
| `pat_att` | A.6 |
| `pat_blocked` | A.6 |
| `pat_made` | A.6 |
| `pat_missed` | A.6 |
| `pat_pct` | A.6 |
| `penalties` | A.4 |
| `penalty_yards` | A.4 |
| `pendiente_calibracion` | 10.2, 10.3 |
| `pff` | B.2 |
| `pff_id` | B.1 |
| `pff_position` | B.1 |
| `pff_status` | B.1 |
| `pfr` | B.2 |
| `pfr_id` | B.1 |
| `player_display_name` | 4 |
| `player_id` | 4 |
| `player_name` | 4 |
| `position` | 4, B.1 |
| `position_group` | 4, B.1 |
| `pt_att` | A.7 |
| `pt_blocked` | A.7 |
| `pt_downed` | A.7 |
| `pt_fair_caught` | A.7 |
| `pt_inside_20` | A.7 |
| `pt_long` | A.7 |
| `pt_net_yards` | A.7 |
| `pt_out_of_bounds` | A.7 |
| `pt_return_tds` | A.7 |
| `pt_return_yards` | A.7 |
| `pt_returned` | A.7 |
| `pt_touchback` | A.7 |
| `pt_yards` | A.7 |
| `punt_return_yards` | A.5 |
| `punt_returns` | A.5 |
| `r2` | 10.2 |
| `r2_oos` | 10.2, 10.3 |
| `racr` | 6 |
| `racr_acotado` | 9 |
| `receiving_10` | 6 |
| `receiving_16` | 6 |
| `receiving_20` | 6 |
| `receiving_2pt_conversions` | 6 |
| `receiving_40` | 6 |
| `receiving_air_yards` | 6 |
| `receiving_epa` | 6 |
| `receiving_first_downs` | 6 |
| `receiving_first_downs_career_avg` | 8 |
| `receiving_fumbles` | 6 |
| `receiving_fumbles_lost` | 6 |
| `receiving_tds` | 5, 6 |
| `receiving_tds_pred` | 10.1 |
| `receiving_tds_real` | 10.1 |
| `receiving_yards` | 5, 6 |
| `receiving_yards_after_catch` | 6 |
| `receiving_yards_after_catch_career_avg` | 8 |
| `receiving_yards_after_catch_season_avg` | 8 |
| `receiving_yards_career_avg` | 8 |
| `receiving_yards_last5_avg` | 8 |
| `receiving_yards_pred` | 10.1 |
| `receiving_yards_real` | 10.1 |
| `receiving_yards_volatilidad5` | 9 |
| `receptions` | 5, 6 |
| `receptions_career_avg` | 8 |
| `receptions_last3_avg` | 8 |
| `receptions_last5_avg` | 8 |
| `receptions_pred` | 10.1 |
| `receptions_real` | 10.1 |
| `receptions_season_avg` | 8 |
| `referee` | B.2 |
| `referencia_prueba` | 10.2, 10.3 |
| `referencia_validacion` | 10.2, 10.3 |
| `result` | B.2 |
| `rmse` | 10.2 |
| `roof` | 9, B.2 |
| `rookie_season` | B.1 |
| `rushing_10` | A.2 |
| `rushing_12` | A.2 |
| `rushing_20` | A.2 |
| `rushing_2pt_conversions` | A.2 |
| `rushing_40` | A.2 |
| `rushing_epa` | A.2 |
| `rushing_first_downs` | A.2 |
| `rushing_fumbles` | A.2 |
| `rushing_fumbles_lost` | A.2 |
| `rushing_tds` | A.2 |
| `rushing_yards` | 6, A.2 |
| `sack_fumbles` | A.1 |
| `sack_fumbles_lost` | A.1 |
| `sack_yards_lost` | A.1 |
| `sacks_suffered` | A.1 |
| `season` | 4, 10.3, B.2 |
| `season_type` | 4 |
| `sesgo` | 10.2, 10.3 |
| `sesgo_media_entrenamiento` | 10.2 |
| `short_name` | B.1 |
| `smart_id` | B.1 |
| `special_teams_tds` | 6 |
| `spread_line` | 9, B.2 |
| `stadium` | B.2 |
| `stadium_id` | B.2 |
| `status` | B.1 |
| `suffix` | B.1 |
| `surface` | 9, B.2 |
| `target` | 10.3 |
| `target_share` | 6 |
| `target_share_last3_avg` | 8 |
| `target_share_last5_avg` | 8 |
| `target_share_season_avg` | 8 |
| `target_share_x_ofensiva_equipo` | 8 |
| `targets` | 6 |
| `targets_last5_avg` | 8 |
| `targets_season_avg` | 8 |
| `targets_volatilidad5` | 9 |
| `team` | 4 |
| `temp` | 9, B.2 |
| `total` | B.2 |
| `total_implicito_equipo` | 8 |
| `total_line` | 9, B.2 |
| `under_odds` | B.2 |
| `valor` | 10.3 |
| `ventana` | 10.3 |
| `ventana_pendiente` | 10.3 |
| `ventana_sesgo` | 10.3 |
| `ventana_valor` | 10.3 |
| `week` | 4, 10.3, B.2 |
| `weekday` | B.2 |
| `weight` | B.1 |
| `wind` | 9, B.2 |
| `wopr` | 6 |
| `wopr_last3_avg` | 8 |
| `wopr_season_avg` | 8 |
| `yards_per_target` | 7.1 |
| `years_of_experience` | B.1 |
