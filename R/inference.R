#' Modelo lineal generalizado con disenio complejo
#' 
#' @description \code{modelo_lineal_generalizado} define el disenio a partir de 
#' los conglomerados, con base en esto obtiene ajusta un modelo lineal usando 
#' minimos cuadrados generalizados.
#' 
#' @param disenio la salida de `configuracion_disenio()` que provee ademas los
#' grupos y las variables en forma de lista
#' @param modelo una expresion sinmbolica escrita como cadena de texto, consulta
#' [stats::formula()] para los detalles de modelos con componentes logaritmicas,
#' cuadraticas, etc.
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
#' 
#' @return una lista cuyas clases son svyglm, glm y lm
#' 
#' @examples
#' r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
#' cd <- configuracion_disenio(r14, "ytotcorh", c("comuna", "sexo"), "expc")
#' modelo_lineal_generalizado(cd, "ytotcorh ~ comuna + sexo")
#' 
#' @export

modelo_lineal_generalizado <- function(disenio, modelo = "ytotcorh ~ sexo") {
  fit <- survey::svyglm(stats::as.formula(modelo), design = disenio$disenio)
  return(fit)
}
