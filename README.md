# casen

<!-- badges: start -->
[![R build status](https://github.com/pachadotdev/casen/workflows/R-CMD-check/badge.svg)](https://github.com/pachadotdev/casen/actions?workflow=R-CMD-check)
[![Codecov test coverage](https://codecov.io/gh/pachadotdev/casen/branch/master/graph/badge.svg)](https://codecov.io/gh/pachadotdev/casen?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/gravity)](https://cran.r-project.org/package=gravity)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/pachadotdev/casen/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pachadotdev/casen/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Acerca de

Proporciona un conjunto de funciones para realizar estadistica 
descriptiva e inferencia con el disenio complejo de la encuesta CASEN.
Adicionalmente provee datasets que permiten armonizar los codigos de comunas
que cambian de acuerdo al anio de realizacion de la encuesta y permite convertir
a los codigos oficiales de SUBDERE. Esta paquete esta documentado intencionalmente
en castellano asciificado para que funcione sin problema en diferentes plataformas.

# Documentacion

La documentacion esta disponible en https://pacha.dev/casen. Se incluyen ejemplos
de uso de las funciones del paquete casen y como se integra con otros paquetes de R.

# Instalacion

Version estable
```
install.packages("casen")
```

Version de desarrollo
```
source("https://install-github.me/pachadotdev/casen")
```

# Encuesta CASEN en formato R

Adicionalmente al paquete casen, estan disponibles las encuestas CASEN en formato
rds. El unico cambio realizado sobre estas fue llevar los nombres de columnas
a formato tidy (todo en minusculas reemplazando puntos y espacios por guion bajo).

Links de descarga:

* https://pacha.dev/casen/data-rds/1990.rds
* https://pacha.dev/casen/data-rds/1992.rds
* https://pacha.dev/casen/data-rds/1994.rds
* https://pacha.dev/casen/data-rds/1996.rds
* https://pacha.dev/casen/data-rds/1998.rds
* https://pacha.dev/casen/data-rds/2000.rds
* https://pacha.dev/casen/data-rds/2003.rds
* https://pacha.dev/casen/data-rds/2006.rds
* https://pacha.dev/casen/data-rds/2009.rds
* https://pacha.dev/casen/data-rds/2011.rds
* https://pacha.dev/casen/data-rds/2013.rds
* https://pacha.dev/casen/data-rds/2015.rds
* https://pacha.dev/casen/data-rds/2017.rds
