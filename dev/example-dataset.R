library(casen)
library(tidyverse)
library(haven)

casen_2017 <- leer_casen("data-raw/2017_spss.rar")

casen_2017_los_rios <- casen_2017 %>% filter(region == 14) %>% select(expr, expc, varstrat, varunit, ytotcorh, sexo, comuna, pobreza)

casen_2017_los_rios <- casen_2017_los_rios %>% 
  filter(!is.na(pobreza), expr > 0, expc > 0)

write_sav(casen_2017_los_rios, "inst/extdata/casen_2017_los_rios.sav")
