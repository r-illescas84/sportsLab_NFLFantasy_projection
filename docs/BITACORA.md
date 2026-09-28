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
