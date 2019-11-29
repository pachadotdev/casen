## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE, tidy = FALSE----
knitr::opts_chunk$set(eval = TRUE)

## ------------------------------------------------------------------------
library(casen)
r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
r14

## ---- eval=FALSE---------------------------------------------------------
#  # todos los anios disponibles
#  descargar_casen()
#  
#  # solo anio 2017
#  descargar_casen(2017)
#  
#  # todos los anios disponibles en carpeta descargas
#  descargar_casen("descargas")
#  
#  # solo anio 2017 en carpeta descargas
#  descargar_casen(2017, "descargas")

## ------------------------------------------------------------------------
# Media, mediana y percentil 70
media_agrupada(r14, "ytotcorh", c("comuna", "sexo"), "expc")
mediana_agrupada(r14, "ytotcorh", c("comuna", "sexo"), "expc")
percentiles_agrupados(r14, "ytotcorh", c("comuna", "sexo"), "expc")

## ------------------------------------------------------------------------
library(dplyr)

# con mutate puedo convertir pobreza a una variable binaria
r14 %>% 
  mutate(pobreza = ifelse(pobreza <= 2, 1, 0)) %>% 
  media_agrupada("pobreza", "comuna", "expc")

## ---- eval=FALSE---------------------------------------------------------
#  # con filter puedo dejar las observaciones de la 10ma region u otra
#  leer_casen("2017_spss.rar") %>%
#    filter(region == 10)

## ------------------------------------------------------------------------
# ytotcorh = b0 + b1 comuna + b2 sexo + e
modelo_lineal_generalizado(r14, "ytotcorh ~ comuna + sexo", "expc")

