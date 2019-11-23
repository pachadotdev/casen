## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE, tidy = FALSE----
knitr::opts_chunk$set(eval = TRUE)

## ------------------------------------------------------------------------
library(casen)
r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
r14

## ---- eval=FALSE---------------------------------------------------------
#  descargar_casen(2017)
#  casen2017 <- leer_casen("2017_spss.rar")

## ------------------------------------------------------------------------
# Promedio salarial por comuna
media_agrupada(r14, "ytotcorh", "comuna", "expc")

# Promedio salarial por comuna y sexo
media_agrupada(r14, "ytotcorh", c("comuna","sexo"), "expc")

## ------------------------------------------------------------------------
# Promedio salarial por comuna
mediana_agrupada(r14, "ytotcorh", "comuna", "expc")

# Promedio salarial por comuna y sexo
mediana_agrupada(r14, "ytotcorh", c("comuna","sexo"), "expc")

## ------------------------------------------------------------------------
library(dplyr)

r14 %>% 
  # mutate es una funcion de dplyr, con esta puedo convertir pobreza a una variable binaria
  mutate(pobreza = ifelse(pobreza <= 2, 1, 0)) %>% 
  # ahora puedo usar media_agrupada directamente
  media_agrupada("pobreza", "comuna", "expc")

