#' Media agrupada usando disenio complejo
#' @param datos un data.frame o tibble con la encuesta CASEN (o un subconjunto acotado a una region, etc)
#' @param variable una columna de tipo numerico, por ejemplo ytotcorh que es la opcion por defecto
#' @param agrupacion una columna de tipo texto/factor, por ejemplo region que es la opcion por defecto
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @importFrom rlang sym syms expr
#' @importFrom magrittr %>%
#' @importFrom dplyr select select_at distinct group_by summarise
#' @importFrom tidyr drop_na
#' @importFrom purrr map_dfr map2 negate
#' @importFrom haven is.labelled
#' @importFrom labelled to_factor
#' @importFrom srvyr as_survey_design survey_mean
#' @importFrom survey degf
#' @return un tibble con las medias agrupadas y su intervalo de confianza
#' @examples
#' r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
#' media_agrupada(r14, "ytotcorh", c("comuna", "sexo"), "expc")
#' @export
media_agrupada <- function(datos, variable = "ytotcorh", agrupacion = "region", peso = "expr", conglomerado = "varunit", estrato = "varstrat") {
  # checks ----
  check_input(datos, variable, agrupacion, peso, conglomerado, estrato)
  
  # compute ----
  d <- clean_data(datos, variable, agrupacion, peso, conglomerado, estrato)
  
  des <- create_design(d, variable, agrupacion, peso, conglomerado, estrato)
  
  d_groups <- unique_groups(d, agrupacion)
  
  estimate <- mean_median(d_groups, des, stat_fun = srvyr::survey_mean, agrupacion, conglomerado, estrato, peso, variable, col_prefix = "media_")
  
  names(estimate) <- gsub("_low", "_inf", names(estimate))
  names(estimate) <- gsub("_upp", "_sup", names(estimate))
  
  check_nas(estimate)
  return(estimate)
}

#' Mediana agrupada usando disenio complejo
#' @param datos un data.frame o tibble con la encuesta CASEN (o un subconjunto acotado a una region, etc)
#' @param variable una columna de tipo numerico, por ejemplo ytotcorh que es la opcion por defecto
#' @param agrupacion una columna de tipo texto/factor, por ejemplo region que es la opcion por defecto
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @return un tibble con las medianas agrupadas y su intervalo de confianza
#' @examples
#' r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
#' mediana_agrupada(r14, "ytotcorh", c("comuna", "sexo"), "expc")
#' @export
mediana_agrupada <- function(datos, variable = "ytotcorh", agrupacion = "region", peso = "expr", conglomerado = "varunit", estrato = "varstrat") {
  # checks ----
  check_input(datos, variable, agrupacion, peso, conglomerado, estrato)
  
  # compute ----
  d <- clean_data(datos, variable, agrupacion, peso, conglomerado, estrato)
  
  des <- create_design(d, variable, agrupacion, peso, conglomerado, estrato)
  
  d_groups <- unique_groups(d, agrupacion)
  
  estimate <- mean_median(d_groups, des, stat_fun = srvyr::survey_median, agrupacion, conglomerado, estrato, peso, variable, col_prefix = "mediana_")
  
  names(estimate) <- gsub("_low", "_inf", names(estimate))
  names(estimate) <- gsub("_upp", "_sup", names(estimate))
  
  check_nas(estimate)
  return(estimate)
}

#' Percentiles agrupados usando disenio complejo
#' @param datos un data.frame o tibble con la encuesta CASEN (o un subconjunto acotado a una region, etc)
#' @param variable una columna de tipo numerico, por ejemplo ytotcorh que es la opcion por defecto
#' @param percentiles percentiles a calcular, si no se especifica calcula todos los percentiles
#' @param agrupacion una columna de tipo texto/factor, por ejemplo region que es la opcion por defecto
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @importFrom rlang sym syms expr
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarise select mutate select_if mutate_if bind_cols everything slice
#' @importFrom purrr map_dfr map2 negate
#' @importFrom haven is.labelled
#' @importFrom labelled to_factor
#' @importFrom srvyr survey_quantile
#' @importFrom survey degf
#' @return un tibble con los percentiles y su error estandar
#' @examples
#' r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
#' percentiles_agrupados(r14, "ytotcorh", c(0.25, 0.5, 0.75), c("comuna", "sexo"), "expc")
#' @export
percentiles_agrupados <- function(datos, variable = "ytotcorh", percentiles = 1:100 / 100, agrupacion = "region", peso = "expr", conglomerado = "varunit", estrato = "varstrat") {
  # checks ----
  check_input(datos, variable, agrupacion, peso, conglomerado, estrato)
  stopifnot(is.numeric(percentiles), length(percentiles) >= 1)

  # compute ----
  d <- clean_data(datos, variable, agrupacion, peso, conglomerado, estrato)
  
  des <- create_design(d, variable, agrupacion, peso, conglomerado, estrato)

  d_groups <- unique_groups(d, agrupacion)
  
  estimate <- suppressWarnings(
    purrr::map_dfr(
      percentiles,
      function(p) {
        purrr::map_dfr(
          seq_len(nrow(d_groups)),
          function(j) {
            filter_values <- d_groups %>% dplyr::slice(j)
            filter_syms <- purrr::map2(names(filter_values), filter_values, ~rlang::expr(!!sym(.x) == !!.y))
            
            des2 <- des %>%
              dplyr::filter(!!!filter_syms)
            
            des2 <- des2 %>% 
              dplyr::group_by(!!!syms(c(agrupacion))) %>% 
              dplyr::summarise(
                !!sym(paste0("mediana_", variable)) := srvyr::survey_quantile(!!sym(variable), quantile = p, df = survey::degf(des2))
              )
            
            des2_1 <- des2 %>% 
              dplyr::select_if(haven::is.labelled) %>% 
              dplyr::mutate_if(haven::is.labelled, labelled::to_factor) %>%
              dplyr::mutate_if(is.factor, as.character)
            
            names(des2_1) <- paste0(names(des2_1), "_etiqueta")
            
            des2_2 <- des2 %>% 
              dplyr::select_if(haven::is.labelled) %>% 
              dplyr::mutate_if(haven::is.labelled, as.character)
            
            names(des2_2) <- paste0(names(des2_2), "_codigo")
            
            des2_3 <- des2 %>% 
              dplyr::select_if(purrr::negate(haven::is.labelled))
            
            des2 <- des2_1 %>% 
              dplyr::bind_cols(des2_2) %>% 
              dplyr::bind_cols(des2_3)
            
            names(des2) <- gsub("_q[1-9][0-9]$|_q100$", "", names(des2))
            names(des2) <- gsub("_q[1-9][0-9]_se$|_q100_se$", "_err_est", names(des2))
            
            des2 <- des2 %>%
              dplyr::mutate(percentil = p) %>%
              dplyr::select("percentil", dplyr::everything())
            
            return(des2)
          }
        )
      }
    )
  )
  
  check_nas(estimate)
  return(estimate)
}
