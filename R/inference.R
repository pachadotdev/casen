#' Modelo lineal generalizado con diseño complejo
#' @description Usa los factores de expansion, conglomerados y estratos para
#' ajustar un modelo lineal generalizado con las variables definidas por el
#' usuario.
#' @param disenio la salida de `configuracion_disenio()` que provee además los
#' grupos y las variables en forma de lista
#' @param modelo una expresion simbólica escrita como cadena de texto, consulta
#' [stats::formula()] para los detalles de modelos con componentes logarítmicas,
#' cuadráticas, etc.
#' @importFrom survey svyglm
#' @return una lista cuyas clases son svyglm, glm y lm
#' @examples
#' cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
#' modelo_lineal_generalizado(cd, "ytotcorh ~ comuna + sexo")
#' @export
modelo_lineal_generalizado <- function(disenio, modelo = "ytotcorh ~ sexo") {
  fit <- survey::svyglm(stats::as.formula(modelo), design = disenio$disenio)
  return(fit)
}
