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
# disenio complejo a partir de  los datos leidos
cd <- configuracion_disenio(r14, "ytotcorh", c("comuna", "sexo"), "expc")
cd$disenio
cd$grupos

## ------------------------------------------------------------------------
# media, mediana y percentil 70
media_agrupada(cd)
mediana_agrupada(cd)
percentiles_agrupados(cd)

## ------------------------------------------------------------------------
library(dplyr)

# convierto pobreza a una variable binaria
r14 %>% 
  mutate(pobreza = ifelse(pobreza <= 2, 1, 0)) %>% 
  configuracion_disenio("pobreza", "comuna", "expc") %>% 
  media_agrupada()

## ---- eval=FALSE---------------------------------------------------------
#  # con filter puedo dejar las observaciones de la 10ma region u otra
#  leer_casen("2017_spss.rar") %>%
#    filter(region == 10)

## ------------------------------------------------------------------------
# modelo: ytotcorh = b0 + b1 comuna + b2 sexo + e
mod <- modelo_lineal_generalizado(cd, "ytotcorh ~ comuna + sexo")
summary(mod)

## ------------------------------------------------------------------------
library(broom)
library(survey)
library(janitor)

# usamos ddf y degf del paquete survey para hacer el mismo calculo
# que realiza Stata
mod_conf <- confint_tidy(mod, ddf = degf(cd$disenio))
mod_conf

## ------------------------------------------------------------------------
# ordenamos la salida del modelo
mod_betas <- tidy(mod)

# pegamos las columnas
mod_betas <- bind_cols(mod_betas, mod_conf)

# ordenamos los nombres de las columnas
mod_betas <- clean_names(mod_betas)

mod_betas

## ------------------------------------------------------------------------
codigos_casen %>% 
  filter(valido_desde == 1990)

## ---- eval=FALSE---------------------------------------------------------
#  casen1990 <- leer_casen("../data-raw/1990_spss.zip") %>%
#    mutate(comu = as.integer(comu)) %>%
#    left_join(
#      codigos_casen %>% filter(valido_desde == 1990) %>% select(starts_with("codigo")),
#      by = c("comu" = "codigo_casen")
#    )

