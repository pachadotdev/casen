## ----setup, cache = FALSE, echo = FALSE, message = FALSE, warning = FALSE, tidy = FALSE----
knitr::opts_chunk$set(eval = TRUE)

## ------------------------------------------------------------------------
library(casen)
casen_2017_los_rios

## ---- eval=FALSE---------------------------------------------------------
#  # todos los anios disponibles en carpeta casen-formato-spss
#  descargar_casen_mds("casen-formato-spss")
#  
#  # solo anio 2017 en carpeta casen-formato-spss
#  descargar_casen_mds(2017, "casen-formato-spss")
#  
#  # leer encuesta CASEN en formato SAV (SPSS)
#  # (se debe descomprimir con winRAR u otro)
#  library(haven)
#  read_sav("casen-formato-spss/2017.sav")

## ---- eval=FALSE---------------------------------------------------------
#  # todos los anios disponibles
#  descargar_casen_github("casen-formato-r")
#  
#  # leer encuesta CASEN en formato RDS (R)
#  readRDS("casen-formato-r/2017.rds")

## ------------------------------------------------------------------------
# disenio complejo a partir de  los datos de ejemplo
cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
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
casen_2017_los_rios %>% 
  mutate(pobreza = ifelse(pobreza <= 2, 1, 0)) %>% 
  configuracion_disenio("pobreza", "comuna", "expc") %>% 
  media_agrupada()

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
#  casen1990 <- read_sav("casen-formato-spss/1990.sav") %>%
#    mutate(comu = as.integer(comu)) %>%
#    left_join(
#      codigos_casen %>% filter(valido_desde == 1990) %>% select(starts_with("codigo")),
#      by = c("comu" = "codigo_casen")
#    )

