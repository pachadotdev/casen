# casen <img src="https://pachamaltese.github.io/casen/hexicon.svg" width=150 align="right" alt="sticker"/>

<!-- badges: start -->
[![R build status](https://github.com/pachamaltese/casen/workflows/R-CMD-check/badge.svg)](https://github.com/pachamaltese/casen/actions?workflow=R-CMD-check)
[![Codecov test coverage](https://codecov.io/gh/pachamaltese/casen/branch/master/graph/badge.svg)](https://codecov.io/gh/pachamaltese/casen?branch=master)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# Acerca de

Proporciona un conjunto de funciones para realizar estadistica 
descriptiva e inferencia con el disenio complejo de la encuesta CASEN.
Adicionalmente provee datasets que permiten armonizar los codigos de comunas
que cambian de acuerdo al anio de realizacion de la encuesta y permite convertir
a los codigos oficiales de SUBDERE. Esta paquete esta documentado intencionalmente
en castellano asciificado para que funcione sin problema en diferentes plataformas.

# Documentacion

La documentacion esta disponible en https://pacha.hk/casen. Se incluyen ejemplos
de uso de las funciones del paquete casen y como se integra con otros paquetes de R.

# Instalacion

Ejecuta el siguiente codigo en RStudio.
```
source("https://install-github.me/pachamaltese/casen")
```

# Encuesta CASEN en formato R

Adicionalmente al paquete casen, estan disponibles las encuestas CASEN en formato
rds. El unico cambio realizado sobre estas fue llevar los nombres de columnas
a formato tidy (todo en minusculas reemplazando puntos y espacios por guion bajo).

Links de descarga:

* https://pachamaltese.github.io/casen/data-rds/1990.rds
* https://pachamaltese.github.io/casen/data-rds/1992.rds
* https://pachamaltese.github.io/casen/data-rds/1994.rds
* https://pachamaltese.github.io/casen/data-rds/1996.rds
* https://pachamaltese.github.io/casen/data-rds/1998.rds
* https://pachamaltese.github.io/casen/data-rds/2000.rds
* https://pachamaltese.github.io/casen/data-rds/2003.rds
* https://pachamaltese.github.io/casen/data-rds/2006.rds
* https://pachamaltese.github.io/casen/data-rds/2009.rds
* https://pachamaltese.github.io/casen/data-rds/2011.rds
* https://pachamaltese.github.io/casen/data-rds/2013.rds
* https://pachamaltese.github.io/casen/data-rds/2015.rds
* https://pachamaltese.github.io/casen/data-rds/2017.rds
