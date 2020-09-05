#' Armoniza los codigos de regiones
#' @description Convierte las etiquetas de las regiones para usar de manera 
#' uniforme los nombres de CASEN 2017. El procedimiento consiste en buscar la 
#' columna \code{r} (antes del anio 2000) o \code{region} (desde el anio 2000) 
#' y aplicar expresiones regulares de acuerdo a la codificacion respectiva.
#' @param d una encuesta CASEN en formato tibble o data.frame
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom dplyr mutate select everything case_when
#' @importFrom labelled to_factor
#' @return un tibble con la columna de region transformada
#' @examples 
#' \donrun{
#' casen1990 <- readRDS("casen1990.rds)
#' armonizar_regiones(casen1990)
#' }
#' @export
armonizar_regiones <- function(d) {
  if (any("r" %in% colnames(d))) {
    d <- d %>% 
      dplyr::mutate(
        !!sym("r") := as.character(labelled::to_factor(!!sym("r")))
      ) %>% 
      dplyr::mutate(
        !!sym("r") := dplyr::case_when(
          r == "I" ~ "Tarapac\u00e1",
          r == "II" ~ "Antofagasta",
          r == "III" ~ "Atacama",
          r == "IV" ~ "Coquimbo",
          r == "V" ~ "Valpara\u00edso",
          r == "VI" ~ "O'Higgins",
          r == "VII" ~ "Maule",
          r == "VIII" ~ "Biob\u00edo",
          r == "IX" ~ "La Araucan\u00eda",
          r == "X" ~ "Los Lagos",
          r == "XI" ~ "Ays\u00e9n",
          r == "XII" ~ "Magallanes",
          r == "R.M." ~ "Metropolitana",
          TRUE ~ r
        )
      ) %>% 
      dplyr::select(!!sym("r"), dplyr::everything())
  }
  
  if (any("region" %in% colnames(d))) {
    d <- d %>% 
      dplyr::mutate(
        !!sym("region") := as.character(labelled::to_factor(!!sym("region")))
      ) %>% 
      dplyr::mutate(
        !!sym("region") := gsub("^.*\\. ", "", !!sym("region")),
        !!sym("region") := gsub("^Regi\u00f3n de |^Regi\u00f3n del |^Regi\u00f3n ", "", !!sym("region"))
      ) %>% 
      dplyr::mutate(
        !!sym("region") := dplyr::case_when(
          !!sym("region") == "Tarapaca" ~ "Tarapac\u00e1",
          grepl("Higgins", !!sym("region")) ==  TRUE ~ "O'Higgins",
          grepl("del Campo", !!sym("region")) ==  TRUE ~ "Ays\u00e9n",
          grepl("Metropolitana", !!sym("region")) == TRUE ~ "Metropolitana",
          grepl("Magallanes", !!sym("region")) ==  TRUE ~ "Magallanes",
          TRUE ~ !!sym("region")
        )
      ) %>% 
      dplyr::select(!!sym("region"), dplyr::everything())
  }
}
