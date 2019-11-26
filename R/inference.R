#' Modelo lineal generalizado con disenio complejo
#' 
#' @description \code{modelo_lineal_generalizado} define el disenio a partir de 
#' los conglomerados, con base en esto obtiene ajusta un modelo lineal usando 
#' minimos cuadrados generalizados.
#' 
#' @param datos un data.frame o tibble con la encuesta CASEN (o un subconjunto 
#' acotado a una region, etc)
#' @param modelo una expresion sinmbolica escrita como cadena de texto, consulta
#' [stats::formula()] para los detalles de modelos con componentes logaritmicas,
#' cuadraticas, etc.
#' @param peso una columna de tipo numerico, por defecto es expr que corresponde
#' al factor de expansion regional de acuerdo al manual CASEN 2017
#' @param conglomerado una columna de tipo numerico, por defecto es varunit de
#' acuerdo al manual CASEN 2017
#' @param estrato una columna de tipo numerico, por defecto es varunit de
#' acuerdo al manual CASEN 2017
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang syms
#' @importFrom tidyr drop_na
#' @importFrom dplyr mutate_if bind_cols
#' @importFrom haven is.labelled
#' @importFrom labelled to_factor
#' @importFrom stats as.formula confint
#' @importFrom srvyr as_survey_design
#' @importFrom survey svyglm degf
#' @importFrom broom tidy confint_tidy glance augment
#' @importFrom janitor clean_names
#' 
#' @return una lista con dos data frames que contienen los betas del modelo y la
#' prediccion asociada 
#' 
#' @examples
#' r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
#' modelo_lineal_generalizado(r14, "ytotcorh ~ comuna + sexo", "expc")
#' 
#' @export

modelo_lineal_generalizado <- function(datos, modelo = "ytotcorh ~ sexo", peso = "expr", conglomerado = "varunit", estrato = "varstrat") {
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
  
  fit_betas <- broom::tidy(fit)
  fit_betas <- dplyr::bind_cols(fit_betas, fit_conf)
  fit_betas <- janitor::clean_names(fit_betas)
  
  names(fit_betas) <- c("termino", "estimacion", "error_estandar", "t_estadistico", "valor_p", "cota_inf_ic", "cota_sup_ic")
  
  fit_predict <- broom::augment(fit)
  
  names(fit_predict) <- gsub("X.weights.", "pesos_x", names(fit_predict))
  names(fit_predict) <- gsub(".fitted", "valor_ajustado", names(fit_predict))
  names(fit_predict) <- gsub(".resid", "residuo", names(fit_predict))
  names(fit_predict) <- gsub(".hat", "sombrero", names(fit_predict))
  names(fit_predict) <- gsub(".sigma", "sigma", names(fit_predict))
  names(fit_predict) <- gsub(".cooksd", "cooks", names(fit_predict))
  names(fit_predict) <- gsub(".std.resid", "residuo_estandarizado", names(fit_predict))
  
  return(
    list(betas = fit_betas,
         prediccion = fit_predict
    )
  )
}
