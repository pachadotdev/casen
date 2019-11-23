#' Modelo lineal generalizado con disenio complejo
#' @param datos un data.frame o tibble con la encuesta CASEN (o un subconjunto acotado a una region, etc)
#' @param variable_dependiente una columna de tipo numerico, por ejemplo ytotcorh que es la opcion por defecto
#' @param variables_independientes una o mas columnas de tipo numerico o factor, por ejemplo region que es la opcion por defecto
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @importFrom magrittr %>%
#' @importFrom rlang syms
#' @importFrom tidyr drop_na
#' @importFrom dplyr mutate_if
#' @importFrom haven is.labelled
#' @importFrom labelled to_factor
#' @importFrom stats as.formula confint
#' @importFrom srvyr select as_survey_design
#' @importFrom survey svyglm degf
#' @importFrom broom tidy confint_tidy
#' @importFrom janitor clean_names
#' @return un tibble con los resultados del modelo lineal generalizado (funcion glm + disenio complejo)
#' @examples
#' r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
#' modelo_lineal_generalizado(r14, "ytotcorh", c("comuna", "sexo"), "expc")
#' @export
modelo_lineal_generalizado <- function(datos, variable_dependiente = "ytotcorh", variables_independientes = "region", peso = "expr", conglomerado = "varunit", estrato = "varstrat") {
  # checks ----
  stopifnot(is.data.frame(datos))
  stopifnot(is.character(variable_dependiente), variable_dependiente %in% colnames(datos), length(variable_dependiente) == 1)
  stopifnot(is.character(variables_independientes), all(variables_independientes %in% colnames(datos)), length(variables_independientes) >= 1)
  stopifnot(is.character(peso), peso %in% colnames(datos), length(peso) == 1)
  stopifnot(is.character(conglomerado), conglomerado %in% colnames(datos), length(conglomerado) == 1)
  stopifnot(is.character(estrato), estrato %in% colnames(datos), length(estrato) == 1)

  # compute ----
  d <- datos %>%
    srvyr::select(!!!syms(c(variable_dependiente, variables_independientes, peso, conglomerado, estrato))) %>%
    tidyr::drop_na() %>%
    dplyr::mutate_if(haven::is.labelled, labelled::to_factor) %>%
    dplyr::mutate_if(is.character, labelled::to_factor)

  des <- d %>%
    srvyr::as_survey_design(ids = !!sym(conglomerado), strata = !!sym(estrato), weights = !!sym(peso))

  vars <- paste(variables_independientes, collapse = " + ")
  form <- stats::as.formula(paste(variable_dependiente, "~", vars))

  fit <- survey::svyglm(form, design = des)

  # uses ddf instead of df to obtain the same confidence interval as in Stata
  fit_conf <- broom::confint_tidy(fit, ddf = survey::degf(des))

  fit <- broom::tidy(fit)
  fit <- dplyr::bind_cols(fit, fit_conf)
  fit <- janitor::clean_names(fit)

  return(fit)
}

#' Modelo lineal generalizado con disenio complejo (version avanzada)
#' @param datos un data.frame o tibble con la encuesta CASEN (o un subconjunto acotado a una region, etc)
#' @param modelo una expresion sinmbolica escrita como cadena de texto, consulta [stats::formula()] para los detalles de modelos con
#' componentes logaritmicas, cuadraticas, etc.
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de acuerdo al manual CASEN 2017
#' @importFrom magrittr %>%
#' @importFrom rlang syms
#' @importFrom tidyr drop_na
#' @importFrom dplyr mutate_if
#' @importFrom haven is.labelled
#' @importFrom labelled to_factor
#' @importFrom stats as.formula confint
#' @importFrom srvyr as_survey_design
#' @importFrom survey svyglm degf
#' @importFrom broom tidy confint_tidy
#' @importFrom janitor clean_names
#' @return un tibble con los resultados del modelo lineal generalizado (funcion glm + disenio complejo)
#' @examples
#' r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
#' modelo_lineal_generalizado_2(r14, "ytotcorh ~ comuna + sexo", "expc")
#' @export
modelo_lineal_generalizado_2 <- function(datos, modelo = "ytotcorh ~ sexo", peso = "expr", conglomerado = "varunit", estrato = "varstrat") {
  # checks ----
  stopifnot(is.data.frame(datos))
  stopifnot(is.character(modelo), length(modelo) == 1)
  stopifnot(is.character(peso), peso %in% colnames(datos), length(peso) == 1)
  stopifnot(is.character(conglomerado), conglomerado %in% colnames(datos), length(conglomerado) == 1)
  stopifnot(is.character(estrato), estrato %in% colnames(datos), length(estrato) == 1)
  
  # compute ----
  des <- datos %>%
    tidyr::drop_na() %>%
    dplyr::mutate_if(haven::is.labelled, labelled::to_factor) %>%
    dplyr::mutate_if(is.character, labelled::to_factor) %>% 
    srvyr::as_survey_design(ids = !!sym(conglomerado), strata = !!sym(estrato), weights = !!sym(peso))
  
  form <- stats::as.formula(modelo)
  
  fit <- survey::svyglm(form, design = des)
  
  # uses ddf instead of df to obtain the same confidence interval as in Stata
  fit_conf <- broom::confint_tidy(fit, ddf = survey::degf(des))
  
  fit <- broom::tidy(fit)
  fit <- dplyr::bind_cols(fit, fit_conf)
  fit <- janitor::clean_names(fit)
  
  return(fit)
}
