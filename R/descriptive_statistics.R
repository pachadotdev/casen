#' Estadistica descriptiva usando disenio complejo
#' 
#' @description \code{estadistica_descriptiva} define el disenio a partir de los
#' conglomerados, estratos y pesos, con base en esto obtiene las medidas de
#' tendencia central
#' 
#' @param datos un data.frame o tibble con la encuesta CASEN (o un subconjunto
#' acotado a una region, etc)
#' @param variable una columna de tipo numerico, por ejemplo ytotcorh que es la
#' opcion por defecto
#' @param agrupacion una columna de tipo texto/factor, por ejemplo region que es
#' la opcion por defecto
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde
#' al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param cuantiles un vector de tipo numerico, por defecto es 1:100 / 100
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de
#' acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de
#' acuerdo al manual CASEN 2017
#' @param solo_media valor logico para omitir el calculo de mediana y cuantiles,
#' por defecto es FALSE
#'  
#' @importFrom rlang sym syms expr
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarise select select_at select_if mutate mutate_if distinct bind_cols everything slice
#' @importFrom tidyr drop_na
#' @importFrom purrr map_dfr map2 negate
#' @importFrom haven is.labelled
#' @importFrom labelled to_factor
#' @importFrom srvyr as_survey_design survey_mean survey_quantile
#' @importFrom survey degf
#' 
#' @return una lista con tres data frames que contienen las medias, medianas y
#' percentiles
#' 
#' @examples
#' r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
#' estadistica_descriptiva(r14, "ytotcorh", c("comuna", "sexo"), "expc", 0.7)
#' 
#' @export

estadistica_descriptiva <- function(datos, variable = "ytotcorh", agrupacion = "region", peso = "expr",
                                    cuantiles = 1:100 / 100, conglomerado = "varunit", estrato = "varstrat",
                                    solo_media = FALSE) {
  # checks ----
  check_input(datos, variable, agrupacion, peso, conglomerado, estrato)
  
  # compute ----
  d <- clean_data(datos, variable, agrupacion, peso, conglomerado, estrato)
  
  des <- create_design(d, variable, agrupacion, peso, conglomerado, estrato)
  
  d_groups <- unique_groups(d, agrupacion)
  
  mean_output <- mean_median(d_groups, des, stat_fun = srvyr::survey_mean, agrupacion, conglomerado, estrato, peso, variable, col_prefix = "media_")
  names(mean_output) <- gsub("_low", "_inf", names(mean_output))
  names(mean_output) <- gsub("_upp", "_sup", names(mean_output))
  
  if (solo_media == F) {
  median_output <- mean_median(d_groups, des, stat_fun = srvyr::survey_median, agrupacion, conglomerado, estrato, peso, variable, col_prefix = "mediana_")
  names(median_output) <- gsub("_low", "_inf", names(median_output))
  names(median_output) <- gsub("_upp", "_sup", names(median_output))
  
  quantile_output <- suppressWarnings(
    purrr::map_dfr(
      cuantiles,
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
                !!sym(paste0("cuantil_", variable)) := srvyr::survey_quantile(!!sym(variable), quantile = p, df = survey::degf(des2))
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
              dplyr::mutate(cuantil = p) %>%
              dplyr::select("cuantil", dplyr::everything())
            
            return(des2)
          }
        )
      }
    )
  )
  
  check_nas(mean_output)
  # check_nas(median_output)
  # check_nas(quantile_output)
  
  return(
    list(
      media = mean_output,
      mediana = median_output,
      cuantiles = quantile_output
    )
  )
  } else {
    return(list(media = mean_output))
  }
}
