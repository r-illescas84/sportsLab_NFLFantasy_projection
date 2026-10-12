"""
Flujo semanal de WR: de las bases de nflreadpy a la prediccion de la semana.

    python weekly/wr/src/pipeline.py --season 2026 --week 5
    python weekly/wr/src/pipeline.py --auto

Con --season y --week corre esa semana. Con --auto decide con el calendario que toca hoy (evaluar
las semanas jugadas sin evaluacion y emitir la siguiente) y lo corre; es el modo que usa el workflow
programado de GitHub Actions (docs/decisions/0007-corrida-automatica.md).

1. data.py         carga las tablas.
2. features.py     arma la tabla de variables; si la semana aun no se juega, agrega antes las
                   filas de los WR a predecir.
3. seguimiento.py  revisa la calidad de la tabla; lo que invalida la prediccion detiene la corrida.
4. Semana pendiente: modeling.py aplica los modelos guardados en weekly/wr/models/ y se guardan
   la prediccion emitida (predicciones_semana_N.csv) y las variables con que se hizo
   (variables_semana_N.csv). Correrla otra vez antes del primer partido reemplaza las dos.
5. Semana jugada: se evalua la prediccion emitida, sin modificarla (si no existe, se recalcula y
   queda marcada como reconstruida): evaluacion_semana_N.csv (prediccion y real) y
   metricas_semana_N.csv, mas la ventana de las ultimas 4 semanas contra los limites de control.
6. Cada corrida agrega sus filas a weekly/wr/outputs/tracking.csv.

No entrena nada: los modelos se deciden y evaluan en los notebooks (experimentos.py), y cuando se
reentrenan esta escrito en docs/decisions/0006-seguimiento-semanal.md.
"""
import argparse
import json
import sys
from pathlib import Path

import pandas as pd

import data
import features
import modeling
import seguimiento

COLUMNA_ORDEN = "receiving_yards_pred"
"""Columna por la que se ordenan la prediccion y la evaluacion de cada semana."""

RAIZ = Path(__file__).resolve().parents[1]
CARPETA_MODELOS = RAIZ / "models"
CARPETA_SALIDAS = RAIZ / "outputs"

DIAS_PARA_EMITIR = 5
"""--auto emite una semana solo si su primer partido es dentro de estos dias. Las semanas 2 a 18 ya
quedan acotadas porque la anterior tiene que haberse jugado; el limite es para la semana 1, que no
tiene semana anterior: sin el, se emitiria desde principios de septiembre, antes de que esten las
alineaciones. Cinco dias cubren del martes al domingo."""

CODIGO_DETENIDA = 3
"""Codigo de salida cuando un chequeo de calidad detuvo la corrida (seguimiento.ChequeoBloqueante);
cualquier otro error sale con el codigo normal de Python."""

CHEQUEO_EQUIPOS = "cada equipo que juega tiene receptores"
"""En una semana jugada este chequeo solo advierte: no se sabe si la fuente publica las estadisticas
de todos los partidos al mismo tiempo. Si falla al evaluar, _correr() pide avisar para volver a
correr la semana cuando esten completas."""


def juegos_de_la_semana(season, week):
    """Partidos de la semana en el calendario (temporada regular)."""
    juegos = data.cargar_calendario([season])
    juegos = juegos[juegos["week"] == week]
    if juegos.empty:
        raise ValueError(f"no hay partidos en el calendario de la semana {week} de {season}")
    return juegos


def _estado(juegos):
    """'pendiente' si no se ha jugado ningun partido, 'jugada' si ya se jugaron todos y 'en curso'
    si va a medias."""
    jugados = int(juegos["home_score"].notna().sum())
    return "pendiente" if jugados == 0 else "jugada" if jugados == len(juegos) else "en curso"


def estado_de_la_semana(season, week, juegos=None):
    """Regresa ('pendiente' | 'jugada', equipos que juegan esa semana). Una semana a medias no
    se procesa: mezclaria filas reales con filas por predecir de los mismos equipos."""
    if juegos is None:
        juegos = juegos_de_la_semana(season, week)
    estado = _estado(juegos)
    if estado == "en curso":
        jugados = int(juegos["home_score"].notna().sum())
        raise ValueError(
            f"la semana {week} de {season} esta en curso ({jugados} de {len(juegos)} partidos jugados): "
            "correr antes de que empiece o cuando termine"
        )
    return estado, set(juegos["home_team"]) | set(juegos["away_team"])


def tabla_de_la_semana(season, week, estado):
    """Filas de la semana con sus variables (las de la alineacion si esta pendiente, las reales si
    ya se jugo) y las filas reales anteriores a ella, que son la referencia de los chequeos."""
    filas_extra = features.filas_de_la_semana(season, week) if estado == "pendiente" else None
    tabla = features.construir_tabla_modelado(range(2016, season + 1), filas_extra=filas_extra)
    semana = tabla[(tabla["season"] == season) & (tabla["week"] == week)]
    anterior = (tabla["season"] < season) | ((tabla["season"] == season) & (tabla["week"] < week))
    return semana, tabla[anterior & ~tabla["es_prediccion"]]


def _fila_base(season, week, estado, target, modelo, ahora):
    return {"fecha_corrida": ahora, "season": season, "week": week, "estado": estado, "target": target,
            "modelo_entrenado_con": modelo["entrenado_con"], "modelo_fecha_guardado": modelo["fecha_de_guardado"],
            "metrica_principal": modelo["metrica_principal"]}


def _emitir(semana, variables, modelos, destino, season, week, chequeos, ahora):
    """Semana pendiente: guarda las variables y predice a partir de ese mismo archivo, asi lo
    guardado es exactamente lo que produjo la prediccion."""
    ruta_variables = destino / f"variables_semana_{week}.csv"
    semana[seguimiento.COLUMNAS_ID + variables].sort_values("player_id").to_csv(ruta_variables, index=False)
    tabla_variables = pd.read_csv(ruta_variables)
    predicciones = modeling.predecir(tabla_variables, modelos).round(3)
    predicciones = predicciones.sort_values(COLUMNA_ORDEN, ascending=False)
    predicciones.to_csv(destino / f"predicciones_semana_{week}.csv", index=False)

    huella = seguimiento.huella(ruta_variables)
    texto_advertencias = seguimiento.advertencias(chequeos)
    filas = [{**_fila_base(season, week, "pendiente", target, modelo, ahora), "origen_prediccion": "emitida",
              "n_wr": len(predicciones), "huella_variables": huella, "advertencias": texto_advertencias}
             for target, modelo in modelos.items()]
    return {"predicciones": predicciones, "filas": filas}


def _evaluar(semana, modelos, destino, season, week, chequeos, tracking, ahora):
    """Semana jugada: evalua la prediccion emitida (o una reconstruida), sin tocar la emitida."""
    reales = semana[seguimiento.COLUMNAS_ID + list(modeling.TARGETS)].rename(
        columns={t: f"{t}_real" for t in modeling.TARGETS})
    emitida = seguimiento.prediccion_emitida(destino, week)
    n_sin_real = n_sin_prediccion = 0
    huella = None
    if emitida is not None:
        origen = "emitida"
        cruce = emitida.merge(reales.drop(columns=["player_display_name", "team", "season", "week"]),
                              on="player_id", how="outer", indicator=True)
        n_sin_real = int((cruce["_merge"] == "left_only").sum())
        n_sin_prediccion = int((cruce["_merge"] == "right_only").sum())
        evaluacion = cruce[cruce["_merge"] == "both"].drop(columns="_merge")
        huella = seguimiento.huella(destino / f"variables_semana_{week}.csv")
        emision = tracking[(tracking["season"] == season) & (tracking["week"] == week)
                           & (tracking["estado"] == "pendiente")]
        emision = emision[emision["fecha_corrida"] == emision["fecha_corrida"].max()]
        guardados = dict(zip(emision["target"], emision["modelo_fecha_guardado"]))
        otros = [t for t, modelo in modelos.items() if t in guardados and guardados[t] != modelo["fecha_de_guardado"]]
        chequeos = pd.concat([chequeos, seguimiento.resultado_vigencia(
            "prediccion emitida con los modelos vigentes", otros,
            f"{', '.join(otros)} se emitio con otros modelos; las referencias son las de los vigentes" if otros else "")])
    else:
        origen = "reconstruida"
        evaluacion = modeling.predecir(semana, modelos).round(3).merge(
            reales[["player_id"] + [f"{t}_real" for t in modeling.TARGETS]], on="player_id")
    chequeos = pd.concat([chequeos, seguimiento.resultado_vigencia(
        "prediccion emitida encontrada", origen == "reconstruida",
        "se recalculo con los modelos vigentes" if origen == "reconstruida" else "")])
    evaluacion["origen_prediccion"] = origen
    evaluacion = evaluacion.sort_values(COLUMNA_ORDEN, ascending=False)
    evaluacion.to_csv(destino / f"evaluacion_semana_{week}.csv", index=False)

    metricas = modeling.evaluar_predicciones(
        evaluacion, evaluacion.rename(columns=lambda c: c.removesuffix("_real")), modelos)
    metricas.round(4).to_csv(destino / f"metricas_semana_{week}.csv")

    semanas_ventana = range(week - seguimiento.VENTANA_SEMANAS + 1, week + 1)
    rutas = [destino / f"evaluacion_semana_{w}.csv" for w in semanas_ventana]
    ventana_completa = all(r.exists() for r in rutas)
    valores_ventana = (seguimiento.metricas_ventana(pd.concat([pd.read_csv(r) for r in rutas]), modelos)
                       if ventana_completa else None)
    previas = seguimiento.ultimas_corridas(tracking)
    previas = previas[(previas["season"] == season) & (previas["week"] == week - seguimiento.VENTANA_SEMANAS)
                      & (previas["estado"] == "jugada")]

    texto_advertencias = seguimiento.advertencias(chequeos)
    filas = []
    for target, modelo in modelos.items():
        m = metricas.loc[target]
        fila = {**_fila_base(season, week, "jugada", target, modelo, ahora), "origen_prediccion": origen,
                "n_wr": int(m["n"]), "n_predichos_sin_real": n_sin_real, "n_reales_sin_prediccion": n_sin_prediccion,
                "huella_variables": huella, "valor": m[modelo["metrica_principal"]],
                "referencia_validacion": m["referencia_validacion"], "referencia_prueba": m["referencia_prueba"],
                "r2_oos": m["r2_oos"], "sesgo": m["sesgo"], "pendiente_calibracion": m["pendiente_calibracion"],
                "advertencias": texto_advertencias}
        if valores_ventana is not None:
            valores = valores_ventana[target]
            previa = previas.loc[previas["target"] == target, "alertas"]
            fila.update({"ventana": f"{semanas_ventana[0]}-{semanas_ventana[-1]}",
                         "ventana_valor": valores[modelo["metrica_principal"]], "ventana_sesgo": valores["sesgo"],
                         "ventana_pendiente": valores["pendiente_calibracion"]})
            if modelo["limites_seguimiento"] is not None:
                fila["alertas"] = seguimiento.evaluar_alertas(
                    valores, modelo["limites_seguimiento"], previa.iloc[0] if len(previa) else "")
        filas.append(fila)
    return {"predicciones": evaluacion, "metricas": metricas, "filas": filas}


def ejecutar_semana(season, week, carpeta_modelos=CARPETA_MODELOS, carpeta_salidas=CARPETA_SALIDAS):
    juegos = juegos_de_la_semana(season, week)
    estado, _ = estado_de_la_semana(season, week, juegos)
    modelos = modeling.cargar_modelos(carpeta_modelos)
    variables = list(dict.fromkeys(v for modelo in modelos.values() for v in modelo["variables"]))

    semana, historico = tabla_de_la_semana(season, week, estado)
    chequeos = seguimiento.chequear_calidad(semana, historico, variables, estado, juegos)
    seguimiento.detener_si_bloquea(chequeos)

    destino = Path(carpeta_salidas) / str(season)
    destino.mkdir(parents=True, exist_ok=True)
    ruta_tracking = Path(carpeta_salidas) / "tracking.csv"
    ahora = pd.Timestamp.now(tz="UTC").isoformat(timespec="seconds")
    if estado == "pendiente":
        resultado = _emitir(semana, variables, modelos, destino, season, week, chequeos, ahora)
        resultado["metricas"] = None
    else:
        resultado = _evaluar(semana, modelos, destino, season, week, chequeos,
                             seguimiento.leer_tracking(ruta_tracking), ahora)
    filas = seguimiento.registrar_corrida(resultado["filas"], ruta_tracking)

    return {
        "estado": estado,
        "predicciones": resultado["predicciones"],
        "metricas": resultado["metricas"],
        "chequeos": chequeos,
        "tracking": filas,
        "entrenado_con": modelos[modeling.TARGETS[0]]["entrenado_con"],
    }


def temporada_del_calendario(hoy):
    """Temporada regular en curso segun la fecha: de septiembre a diciembre, la del año; en enero,
    la anterior, porque la temporada regular termina en enero. De febrero a agosto no hay temporada
    regular y regresa None. No se usa nflreadpy.get_current_season(): cambia de temporada el jueves
    despues del Labor Day, y en 2026 la temporada empezo un dia antes, el miercoles 9 de septiembre."""
    hoy = pd.Timestamp(hoy)
    if hoy.month >= 9:
        return hoy.year
    return hoy.year - 1 if hoy.month == 1 else None


def estados_de_la_temporada(season):
    """Una fila por semana de la temporada regular, con una sola carga del calendario: su estado
    ('pendiente', 'en curso' o 'jugada') y la fecha de su primer partido."""
    calendario = data.cargar_calendario([season])
    return pd.DataFrame([{"week": int(week), "estado": _estado(juegos), "primer_partido": juegos["gameday"].min()}
                         for week, juegos in calendario.groupby("week")])


def semanas_evaluadas(tracking, season):
    """Semanas de la temporada que ya tienen una evaluacion en el historial."""
    jugadas = tracking[(tracking["season"] == season) & (tracking["estado"] == "jugada")]
    return set(jugadas["week"].astype(int))


def acciones_pendientes(estados, evaluadas, hoy):
    """Lo que toca correr hoy, en orden: evaluar cada semana jugada que todavia no tiene evaluacion
    y emitir la primera semana pendiente, si la anterior ya se jugo (o es la semana 1) y su primer
    partido es dentro de DIAS_PARA_EMITIR dias. Una semana en curso no se toca, ni se emite la
    siguiente. `estados` viene de estados_de_la_temporada(), `evaluadas` de semanas_evaluadas() y
    `hoy` es una fecha. Regresa una lista de ('evaluar' | 'emitir', semana)."""
    estados = estados.sort_values("week")
    acciones = [("evaluar", int(w)) for w in estados.loc[estados["estado"] == "jugada", "week"] if w not in evaluadas]
    pendientes = estados[estados["estado"] == "pendiente"]
    if not pendientes.empty:
        siguiente = pendientes.iloc[0]
        anterior = estados.loc[estados["week"] == siguiente["week"] - 1, "estado"]
        anterior_jugada = anterior.empty or anterior.iloc[0] == "jugada"
        dias = (pd.Timestamp(siguiente["primer_partido"]) - pd.Timestamp(hoy)).days
        if anterior_jugada and dias <= DIAS_PARA_EMITIR:
            acciones.append(("emitir", int(siguiente["week"])))
    return acciones


def _correr(season, semanas, ultima, carpeta_modelos, carpeta_salidas):
    """Corre las semanas en orden con ejecutar_semana y junta los eventos que piden avisar: corrida
    detenida, estadisticas incompletas al evaluar, alerta persistente y fin de la temporada regular
    (`ultima` es su ultima semana). Si un chequeo detiene una semana no sigue con las demas: la
    siguiente corrida las reintenta."""
    hechas, eventos = [], []
    for week in semanas:
        try:
            resultado = ejecutar_semana(season, week, carpeta_modelos, carpeta_salidas)
        except seguimiento.ChequeoBloqueante as error:
            eventos.append({
                "titulo": f"Corrida detenida: semana {week} de {season}",
                "cuerpo": f"{error}\n\nNo se escribio nada. La corrida programada la reintenta si esa semana le toca; "
                "si no, volver a correrla a mano cuando se resuelva."})
            return {"season": season, "hechas": hechas, "eventos": eventos, "detenida": str(error)}
        hechas.append({"week": week, **resultado})
        if resultado["estado"] != "jugada":
            continue
        chequeos = resultado["chequeos"]
        incompletas = chequeos[(chequeos["chequeo"] == CHEQUEO_EQUIPOS) & ~chequeos["ok"]]
        if not incompletas.empty:
            eventos.append({
                "titulo": f"Estadisticas incompletas: semana {week} de {season}",
                "cuerpo": f"{incompletas['detalle'].iloc[0]}.\n\nLa semana se evaluo con los datos disponibles. Cuando "
                "esten completos, volver a correrla: en GitHub, Actions > Corrida semanal WR > Run workflow, con "
                f"season {season}, week {week} y publicar; o en local, python weekly/wr/src/pipeline.py --season "
                f"{season} --week {week}. La nueva evaluacion reemplaza a esta en las tablas y alertas."})
        alertas = resultado["tracking"].dropna(subset=["alertas"])
        persistentes = alertas[alertas["alertas"].astype(str).str.contains("(persistente)", regex=False)]
        if not persistentes.empty:
            eventos.append({
                "titulo": f"Alerta persistente: semana {week} de {season}",
                "cuerpo": "\n".join(f"- {f.target}: {f.alertas}" for f in persistentes.itertuples())
                + "\n\nRevisar el modelo antes de la siguiente corrida (docs/decisions/0006-seguimiento-semanal.md). "
                "Si la revision lo confirma, se reentrena con weekly/wr/notebooks/6.1_modelo_final_y_temporada_actual.ipynb."})
        if week == ultima:
            eventos.append({
                "titulo": f"Fin de la temporada regular {season}: reentrenar",
                "cuerpo": f"Se evaluo la semana {week}, la ultima de la temporada regular. Toca reentrenar con la "
                "temporada completa: re-ejecutar weekly/wr/notebooks/6.1_modelo_final_y_temporada_actual.ipynb, "
                "que tambien recalcula los limites de las alertas (docs/decisions/0006-seguimiento-semanal.md)."})
    return {"season": season, "hechas": hechas, "eventos": eventos, "detenida": None}


def ejecutar_automatico(hoy=None, carpeta_modelos=CARPETA_MODELOS, carpeta_salidas=CARPETA_SALIDAS):
    """Modo --auto: con el calendario y el historial decide que toca hoy (acciones_pendientes) y lo
    corre. Fuera de la temporada regular no hace nada."""
    hoy = pd.Timestamp(hoy) if hoy is not None else pd.Timestamp.now(tz="UTC").tz_localize(None).normalize()
    season = temporada_del_calendario(hoy)
    if season is None:
        return {"season": None, "acciones": [], "hechas": [], "eventos": [], "detenida": None}
    estados = estados_de_la_temporada(season)
    tracking = seguimiento.leer_tracking(Path(carpeta_salidas) / "tracking.csv")
    acciones = acciones_pendientes(estados, semanas_evaluadas(tracking, season), hoy)
    salida = _correr(season, [week for _, week in acciones], int(estados["week"].max()), carpeta_modelos, carpeta_salidas)
    return {**salida, "acciones": acciones}


def ejecutar_manual(season, week, carpeta_modelos=CARPETA_MODELOS, carpeta_salidas=CARPETA_SALIDAS):
    """Una semana elegida a mano, con la misma salida que ejecutar_automatico()."""
    ultima = int(data.cargar_calendario([season])["week"].max())
    return {**_correr(season, [week], ultima, carpeta_modelos, carpeta_salidas), "acciones": [("manual", week)]}


def _tabla_markdown(tabla):
    lineas = ["| " + " | ".join(tabla.columns) + " |", "|" + " --- |" * len(tabla.columns)]
    lineas += ["| " + " | ".join(str(v) for v in fila) + " |" for fila in tabla.itertuples(index=False)]
    return "\n".join(lineas)


def resumen_markdown(salida):
    """Resumen de la corrida en markdown, para el workflow. Su primera linea, sin el '## ', sirve de
    mensaje del commit de las salidas."""
    season = salida["season"]
    if season is None:
        return f"## Corrida semanal {features.POSICION}: fuera de la temporada regular, nada que hacer\n"
    partes = [f"semana {h['week']} {'emitida' if h['estado'] == 'pendiente' else 'evaluada'}" for h in salida["hechas"]]
    if salida["detenida"]:
        partes.append("corrida detenida")
    lineas = [f"## Corrida semanal {features.POSICION} {season}: {', '.join(partes) or 'nada que hacer hoy'}", ""]
    for h in salida["hechas"]:
        filas = h["tracking"]
        if h["estado"] == "pendiente":
            top = h["predicciones"].head(10)[["player_display_name", "team"] + [f"{t}_pred" for t in modeling.TARGETS]]
            lineas += [f"### Semana {h['week']}: prediccion emitida", "",
                       f"{len(h['predicciones'])} receptores. Los 10 con mas yardas esperadas:", "", _tabla_markdown(top), ""]
        else:
            tabla = filas[["target", "metrica_principal", "valor", "referencia_validacion", "referencia_prueba",
                           "ventana", "alertas"]].copy()
            for columna in ("valor", "referencia_validacion", "referencia_prueba"):
                tabla[columna] = tabla[columna].astype(float).round(4)
            lineas += [f"### Semana {h['week']}: evaluacion de la prediccion {filas['origen_prediccion'].iloc[0]}", "",
                       _tabla_markdown(tabla.fillna("")), ""]
        advertencias = filas["advertencias"].dropna().astype(str)
        lineas += [f"Advertencias de calidad: {advertencias.iloc[0] if len(advertencias) and advertencias.iloc[0] else 'ninguna'}", ""]
    if salida["detenida"]:
        lineas += ["### Corrida detenida", "", "```", salida["detenida"], "```", "",
                   "No se escribio nada. La corrida programada la reintenta si esa semana le toca; si no, volver a "
                   "correrla a mano cuando se resuelva.", ""]
    return "\n".join(lineas)


def _imprimir(season, h):
    print(f"\nsemana {h['week']} de {season}: {h['estado']}, {len(h['predicciones'])} {features.POSICION}")
    print(f"modelos entrenados con: {h['entrenado_con']}")
    texto = seguimiento.advertencias(h["chequeos"])
    print(f"advertencias de calidad: {texto or 'ninguna'}")
    print(h["predicciones"].head(10).to_string(index=False))
    if h["metricas"] is not None:
        print("\nmetricas contra lo real:")
        print(h["metricas"].round(4).to_string())
        columnas = ["target", "ventana", "ventana_valor", "ventana_sesgo", "ventana_pendiente", "alertas"]
        print("\nventana de las ultimas semanas y alertas:")
        print(h["tracking"][columnas].fillna("").to_string(index=False))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=f"Prediccion semanal de {features.POSICION}")
    parser.add_argument("--season", type=int)
    parser.add_argument("--week", type=int)
    parser.add_argument("--auto", action="store_true", help="decidir con el calendario que toca hoy")
    parser.add_argument("--resumen", type=Path, help="escribir aqui el resumen de la corrida, en markdown")
    parser.add_argument("--eventos", type=Path, help="escribir aqui, en JSON, los eventos que piden avisar")
    args = parser.parse_args()
    if args.auto and (args.season is not None or args.week is not None):
        parser.error("--auto no lleva --season ni --week")
    if not args.auto and (args.season is None or args.week is None):
        parser.error("hace falta --season y --week, o --auto")

    if args.auto:
        salida = ejecutar_automatico()
        acciones = ", ".join(f"{accion} la semana {week}" for accion, week in salida["acciones"])
        print(f"temporada {salida['season']}: {acciones or 'nada que hacer hoy'}")
    else:
        salida = ejecutar_manual(args.season, args.week)
    for hecha in salida["hechas"]:
        _imprimir(salida["season"], hecha)

    for ruta, contenido in ((args.resumen, resumen_markdown(salida)),
                            (args.eventos, json.dumps(salida["eventos"], ensure_ascii=False, indent=2))):
        if ruta:
            ruta.parent.mkdir(parents=True, exist_ok=True)
            ruta.write_text(contenido)
    if salida["detenida"]:
        print(salida["detenida"], file=sys.stderr)
        sys.exit(CODIGO_DETENIDA)
