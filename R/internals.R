#' Verifica la entrada del usuario
#' @param datos un data.frame o tibble con la encuesta CASEN (o un subconjunto acotado a una region, etc)
#' @param variable una columna de tipo numerico, por ejemplo ytotcorh que es la opcion por defecto
#' @param agrupacion una columna de tipo texto/factor, por ejemplo region que es la opcion por defecto
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @keywords internal
#' @export
check_input <- function(datos, variable, agrupacion, peso, conglomerado, estrato) {
  stopifnot(is.data.frame(datos))
  stopifnot(is.character(variable), variable %in% colnames(datos), length(variable) == 1)
  stopifnot(is.character(agrupacion), all(agrupacion %in% colnames(datos)), length(agrupacion) >= 1)
  stopifnot(is.character(peso), peso %in% colnames(datos), length(peso) == 1)
  stopifnot(is.character(conglomerado), conglomerado %in% colnames(datos), length(conglomerado) == 1)
  stopifnot(is.character(estrato), estrato %in% colnames(datos), length(estrato) == 1)
}

#' Elimina las observaciones con NA en variables de agrupación
#' @param datos un data.frame o tibble con la encuesta CASEN (o un subconjunto acotado a una region, etc)
#' @param variable una columna de tipo numerico, por ejemplo ytotcorh que es la opcion por defecto
#' @param agrupacion una columna de tipo texto/factor, por ejemplo region que es la opcion por defecto
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @importFrom magrittr %>%
#' @importFrom rlang syms
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @keywords internal
#' @export
clean_data <- function(datos, variable, agrupacion, peso, conglomerado, estrato) {
  d <- datos %>%
    dplyr::select(!!!syms(c(variable, agrupacion, peso, conglomerado, estrato))) %>%
    tidyr::drop_na(!!!syms(agrupacion))

  return(d)
}

#' Crea un objeto de diseño complejo a partir de un data frame
#' @param d un data.frame o tibble con la encuesta CASEN (o un subconjunto acotado a una region, etc)
#' @param variable una columna de tipo numerico, por ejemplo ytotcorh que es la opcion por defecto
#' @param agrupacion una columna de tipo texto/factor, por ejemplo region que es la opcion por defecto
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom srvyr as_survey_design
#' @keywords internal
#' @export
create_design <- function(d, variable, agrupacion, peso, conglomerado, estrato) {
  d <- d %>% 
    srvyr::as_survey_design(
      ids = !!sym(conglomerado), strata = !!sym(estrato), weights = !!sym(peso)
    )
  
  return(d)
}

#' Obtiene los grupos únicos a partir de las variables de agrupación
#' Extract unique groups from grouping variables combinations
#' @param d un data.frame o tibble
#' @param agrupacion columnas de tipo teto/factor en d
#' @importFrom magrittr %>%
#' @importFrom dplyr select_at distinct
#' @keywords internal
#' @export
unique_groups <- function(d, agrupacion) {
  d <- d %>%
    dplyr::select_at(agrupacion) %>%
    dplyr::distinct()

  return(d)
}

#' Verifica la presencia de NAs en los intervalos de confianza
#' @param d un data.frame o tibble con un estadistico agrupado
#' @importFrom magrittr %>%
#' @importFrom srvyr summarise_if
#' @keywords internal
#' @export
check_nas <- function(d) {
  d_aux <- d %>%
    srvyr::summarise_if(is.numeric, sum) %>%
    as.matrix()

  if (any(!is.finite(d_aux))) {
    warning("Con la agregacion proporcionada algunas observaciones tienen cero
    grados de libertad, por lo que no se puede estimar la varianza, dando como
    resultado algunos elementos no finitos.")
  }
}

#' Envolvente para estadistica descriptiva con diseños complejos
#' @param d_groups un vector que sea la salida de unique_tuples()
#' @param des disenio de encuesta a partir de un data.frame o tibble
#' @param stat_fun la función a usar para la agregacion, por ejemplo srvyr::survey_mean()
#' @param agrupacion una columna de tipo texto/factor, por ejemplo region que es la opcion por defecto
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param variable una columna de tipo numerico, por ejemplo ytotcorh que es la opcion por defecto
#' @param col_prefix prefijo para agregar a las columnas resultantes
#' @importFrom rlang sym syms expr :=
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarise select_if mutate_if bind_cols slice
#' @importFrom purrr map_dfr map2 negate
#' @importFrom haven is.labelled
#' @importFrom labelled to_factor
#' @importFrom srvyr survey_mean survey_median
#' @importFrom survey degf
#' @keywords internal
#' @export
mean_median <- function(d_groups, des, stat_fun, agrupacion, conglomerado, estrato, peso, variable, col_prefix) {
  suppressWarnings(purrr::map_dfr(
    seq_len(nrow(d_groups)),
    function(j) {
      filter_values <- d_groups %>% dplyr::slice(j)
      filter_syms <- purrr::map2(names(filter_values), filter_values, ~rlang::expr(!!sym(.x) == !!.y))
      
      des2 <- des %>%
        dplyr::filter(!!!filter_syms)
      
      des2 <- des2 %>% 
        dplyr::group_by(!!!syms(c(agrupacion))) %>% 
        dplyr::summarise(
          !!sym(paste0(col_prefix, variable)) := stat_fun(!!sym(variable), vartype = "ci", df = survey::degf(des2))
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
      
      return(des2)
    }
  ))
}
