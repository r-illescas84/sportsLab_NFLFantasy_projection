"""
Flujo semanal de WR: de las bases de nflreadpy a la prediccion de la semana.

    python weekly/wr/src/pipeline.py --season 2026 --week 4

1. data.py      carga las tablas.
2. features.py  arma la tabla de variables; si la semana aun no se juega, agrega antes las
                filas de los WR a predecir.
3. modeling.py  carga los modelos guardados en weekly/wr/models/ y predice.
4. Si la semana ya se jugo, compara contra lo real y trae las metricas.
5. Guarda en weekly/wr/outputs/{season}/.

No entrena nada: los modelos se deciden y evaluan en los notebooks (experimentos.py).
"""
import argparse
from pathlib import Path

import data
import features
import modeling

RAIZ = Path(__file__).resolve().parents[1]
CARPETA_MODELOS = RAIZ / "models"
CARPETA_SALIDAS = RAIZ / "outputs"


def estado_de_la_semana(season, week):
    """Regresa ('pendiente' | 'jugada', equipos que juegan esa semana). 'pendiente' si no
    se ha jugado ningun partido, 'jugada' si ya se jugaron todos. Una semana a medias no
    se procesa: mezclaria filas reales con filas por predecir de los mismos equipos."""
    juegos = data.cargar_calendario([season])
    juegos = juegos[juegos["week"] == week]
    if juegos.empty:
        raise ValueError(f"no hay partidos en el calendario de la semana {week} de {season}")
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


def ejecutar_semana(season, week, carpeta_modelos=CARPETA_MODELOS, carpeta_salidas=CARPETA_SALIDAS):
    estado, equipos = estado_de_la_semana(season, week)
    filas_extra = None
    if estado == "pendiente":
        filas_extra = features.filas_de_la_semana(season, week)
        sin_alineacion = equipos - set(filas_extra["team"])
        if sin_alineacion:
            raise ValueError(
                f"la alineacion de la semana {week} de {season} esta incompleta: faltan "
                f"{len(sin_alineacion)} de {len(equipos)} equipos ({', '.join(sorted(sin_alineacion))}). "
                "Correr cuando ya haya terminado la semana anterior."
            )
    tabla = features.construir_tabla_modelado(range(2016, season + 1), filas_extra=filas_extra)
    semana = tabla[(tabla["season"] == season) & (tabla["week"] == week)]

    modelos = modeling.cargar_modelos(carpeta_modelos)
    predicciones = modeling.predecir(semana, modelos)

    salida = predicciones.round(3)
    metricas = None
    if estado == "jugada":
        metricas = modeling.evaluar_predicciones(predicciones, semana, modelos)
        for target in modeling.TARGETS:
            salida[f"{target}_real"] = semana[target]
    salida = salida.sort_values("receiving_yards_pred", ascending=False)

    destino = Path(carpeta_salidas) / str(season)
    destino.mkdir(parents=True, exist_ok=True)
    salida.to_csv(destino / f"predicciones_semana_{week}.csv", index=False)
    if metricas is not None:
        metricas.round(4).to_csv(destino / f"metricas_semana_{week}.csv")

    return {
        "estado": estado,
        "predicciones": salida,
        "metricas": metricas,
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
    print(resultado["predicciones"].head(10).to_string(index=False))
    if resultado["metricas"] is not None:
        print("\nmetricas contra lo real:")
        print(resultado["metricas"].round(4).to_string())
