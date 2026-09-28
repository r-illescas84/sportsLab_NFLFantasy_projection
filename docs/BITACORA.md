# Bitácora de mantenimiento

Registro de continuidad entre sesiones de trabajo — no es documentación del proyecto (eso va
en los `README.md`). Una entrada corta por sesión: qué se hizo, qué falta.

---

## 2026-09-27

- Rama de trabajo creada: `daniel`.
- Confirmado que `main` es el estado más actualizado del repo.
- Creada la documentación base: este archivo y `docs/decisions/` (ADRs).
- Docker Desktop verificado corriendo (27.5.1 / Compose v2.32).
- Versiones de `scikit-learn`, `xgboost`, `joblib`, `shap` confirmadas y fijadas en
  `docker/requirements.txt`.
- Imágenes construidas (`nflfantasy-python`, `nflfantasy-r`) y verificadas: imports de Python
  y librerías de R cargan bien, y sobreviven a destruir/recrear el contenedor.
- `renv.lock` generado (120 paquetes). Se corrigieron dos fallas reales en `Dockerfile.r`: el
  caché de `renv` no persistía entre contenedores, y el CRAN grabado en el lockfile era un
  snapshot viejo sin la versión de `renv` necesaria.

**Pendiente:** primer commit en la rama `daniel`.

---

## 2026-09-28

- Primer commit hecho en `daniel` y rama pusheada a GitHub.
- ADR `0001-criterios-de-replica.md`: umbrales numéricos de éxito para la réplica.
- Fase 1 (Gen2, Python) completa — 7 notebooks: `weekly_stats`, `season_avg`, `career_avg`,
  `last5_avg`, `player_shares`, `wrs_rec_yds`/`wrs_receptions`/`wrs_rec_tds`, `wr_merge_stats`.
  Todos coinciden contra los archivos reales (exacto o dentro del umbral del ADR). El bug de
  fuga conocido se reprodujo igual (92.0% = 92.0%). 14 hallazgos documentados en
  `docs/HALLAZGOS.md`, con fixes ya probados para los más importantes.
- Fase 1 (Gen1, R) arrancada con QB como muestra (`qb_zama24.csv`, temporada 2023). Hallazgo
  importante: no existe código en el repo que arme los archivos `*_zama.csv` finales — se
  reconstruyó con un script nuevo (`_fase1_replica/gen1_qb/reconstruir_qb_zama24.R`) que
  reutiliza las funciones `.R` originales sin modificarlas. Coincide casi exacto (12/13
  columnas MAE=0.0000). 3 hallazgos nuevos: `gsis_id` mal asignado (Brian Robinson Jr. /
  Brock Purdy), esquema inconsistente en las funciones `.R` cuando un jugador no tiene
  jugadas, y ruta absoluta de Windows quemada en `Total_tds.R`.

Repetido el mismo ejercicio para RB (`rb_zama24.csv`, 87 jugadores), TE (`te_zama24.csv`, 42) y
WR (`wr_completo24.csv`, 111) — todos temporada 2023. Los 3 coinciden exacto contra los
archivos reales (RB necesitó sumar `pass_td` a `total_touchdowns`; WR tiene 1 jugador con datos
duplicados en el archivo real, no en la réplica). Confirmado: los 9 scripts `.R` de soporte
(`RAtt (1).R`, `RushYardsPG.R`, `Rush_Td.R`, `Fumbles.R`, `Targets.R`, `RecTotal.R`,
`RecYrdsPG.R`, `Rec_Td.R`, `20+YrdPlay_Func.R`) son copias idénticas entre QB/RB/WR/TE. 7
hallazgos nuevos documentados (mal mapeo de `gsis_id` en QB y TE, datos duplicados en WR,
`targets_jugador()` truena sin datos, scripts copiados sin adaptar).

**Fase 1 completa (Gen1 + Gen2).** Scripts de reconstrucción en `_fase1_replica/gen1_{qb,rb,te,wr}/`.

**Pendiente:** commit de todo lo avanzado (ADR, `HALLAZGOS.md`, bitácora, scripts de
reconstrucción). Después: arrancar Fase 2 (gobernanza) y Fase 3 (actualización) en paralelo.
