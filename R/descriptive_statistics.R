#' Media agrupada usando diseño complejo
#' @description Usa los factores de expansion, conglomerados y estratos para
#' calcular correctamente las medias comunales o regionales.
#' @param disenio la salida de `configuracion_disenio()` que provee ademas los
#' grupos y las variables en forma de lista
#' @return Una tabla con las medias agrupadas y su intervalo de confianza.
#' @examples
#' cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
#' media_agrupada(cd)
#' @export
media_agrupada <- function(disenio) {
  estimate <- mean_median(disenio$grupos, disenio$disenio,
                          stat_fun = srvyr::survey_mean,
                          disenio$agrupacion, disenio$conglomerado,
                          disenio$estrato, disenio$peso, disenio$variable,
                          col_prefix = "media_")
  names(estimate) <- gsub("_low", "_inf", names(estimate))
  names(estimate) <- gsub("_upp", "_sup", names(estimate))
  check_nas(estimate)
  return(estimate)
}

#' Mediana agrupada usando diseño complejo
#' @description Usa los factores de expansion, conglomerados y estratos para
#' calcular correctamente las medianas comunales o regionales.
#' @param disenio la salida de `configuracion_disenio()` que provee ademas los
#' grupos y las variables en forma de lista
#' @return Una tabla con las medianas agrupadas y su intervalo de confianza.
#' @examples
#' cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
#' mediana_agrupada(cd)
#' @export
mediana_agrupada <- function(disenio) {
  estimate <- mean_median(disenio$grupos, disenio$disenio,
                          stat_fun = srvyr::survey_median,
                          disenio$agrupacion, disenio$conglomerado,
                          disenio$estrato, disenio$peso, disenio$variable,
                          col_prefix = "mediana_")
  names(estimate) <- gsub("_low", "_inf", names(estimate))
  names(estimate) <- gsub("_upp", "_sup", names(estimate))
  check_nas(estimate)
  return(estimate)
}

#' Percentiles agrupados usando diseño complejo
#' @description Usa los factores de expansion, conglomerados y estratos para
#' calcular correctamente los percentiles comunales o regionales.
#' @param disenio la salida de `configuracion_disenio()` que provee ademas los
#' grupos y las variables en forma de lista
#' @param percentiles percentiles a calcular, si no se especifica calcula el
#' percentil 70
#' @importFrom rlang sym syms expr
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarise select mutate select_if mutate_if bind_cols everything slice
#' @importFrom purrr map_dfr map2 negate
#' @importFrom haven is.labelled
#' @importFrom labelled to_factor
#' @importFrom srvyr survey_quantile
#' @importFrom survey degf
#' @return Una tabla con los percentiles y su error estandar.
#' @examples
#' cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
#' percentiles_agrupados(cd, 0.7)
#' @export
percentiles_agrupados <- function(disenio, percentiles = 0.7) {
  estimate <- suppressWarnings(
    purrr::map_dfr(
      percentiles,
      function(p) {
        purrr::map_dfr(
          seq_len(nrow(disenio$grupos)),
          function(j) {
            filter_values <- disenio$grupos %>% dplyr::slice(j)
            filter_syms <- purrr::map2(names(filter_values), filter_values, ~rlang::expr(!!sym(.x) == !!.y))

            des2 <- disenio$disenio %>%
              dplyr::filter(!!!filter_syms)

            des2 <- des2 %>%
              dplyr::group_by(!!!syms(c(disenio$agrupacion))) %>%
              dplyr::summarise(
                !!sym(paste0("mediana_", disenio$variable)) := srvyr::survey_quantile(!!sym(disenio$variable), quantile = p, df = survey::degf(des2))
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
