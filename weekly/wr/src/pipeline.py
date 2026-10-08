"""
Flujo semanal de WR: de las bases de nflreadpy a la prediccion de la semana.

    python weekly/wr/src/pipeline.py --season 2026 --week 5

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
from pathlib import Path

import pandas as pd

import data
import features
import modeling
import seguimiento

RAIZ = Path(__file__).resolve().parents[1]
CARPETA_MODELOS = RAIZ / "models"
CARPETA_SALIDAS = RAIZ / "outputs"


def juegos_de_la_semana(season, week):
    """Partidos de la semana en el calendario (temporada regular)."""
    juegos = data.cargar_calendario([season])
    juegos = juegos[juegos["week"] == week]
    if juegos.empty:
        raise ValueError(f"no hay partidos en el calendario de la semana {week} de {season}")
    return juegos


def estado_de_la_semana(season, week, juegos=None):
    """Regresa ('pendiente' | 'jugada', equipos que juegan esa semana). 'pendiente' si no
    se ha jugado ningun partido, 'jugada' si ya se jugaron todos. Una semana a medias no
    se procesa: mezclaria filas reales con filas por predecir de los mismos equipos."""
    if juegos is None:
        juegos = juegos_de_la_semana(season, week)
    jugados = int(juegos["home_score"].notna().sum())
    equipos = set(juegos["home_team"]) | set(juegos["away_team"])
    if jugados == 0:
        return "pendiente", equipos
    if jugados == len(juegos):
        return "jugada", equipos
    raise ValueError(
        f"la semana {week} de {season} esta en curso ({jugados} de {len(juegos)} partidos jugados): "
        "correr antes de que empiece o cuando termine"
    )


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
    predicciones = predicciones.sort_values("receiving_yards_pred", ascending=False)
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
    evaluacion = evaluacion.sort_values("receiving_yards_pred", ascending=False)
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


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Prediccion semanal de WR")
    parser.add_argument("--season", type=int, required=True)
    parser.add_argument("--week", type=int, required=True)
    args = parser.parse_args()

    resultado = ejecutar_semana(args.season, args.week)
    print(f"semana {args.week} de {args.season}: {resultado['estado']}, "
          f"{len(resultado['predicciones'])} WR")
    print(f"modelos entrenados con: {resultado['entrenado_con']}")
    texto = seguimiento.advertencias(resultado["chequeos"])
    print(f"advertencias de calidad: {texto or 'ninguna'}")
    print(resultado["predicciones"].head(10).to_string(index=False))
    if resultado["metricas"] is not None:
        print("\nmetricas contra lo real:")
        print(resultado["metricas"].round(4).to_string())
        columnas = ["target", "ventana", "ventana_valor", "ventana_sesgo", "ventana_pendiente", "alertas"]
        print("\nventana de las ultimas semanas y alertas:")
        print(resultado["tracking"][columnas].fillna("").to_string(index=False))
