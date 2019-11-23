#' Lee los archivos descargados de la encuesta CASEN
#' @param archivo cadena de texto con la ruta de la encuesta en formato zip/rar o sav/dta
#' @importFrom glue glue
#' @importFrom fs file_exists path_ext
#' @importFrom haven read_sav read_dta
#' @importFrom janitor clean_names
#' @return la encuesta CASEN en formato tibble
#' @examples
#' # datos de ejemplo en el paquete
#' leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
#'
#' # descargando los datos
#' # descargar_casen(2017, "data-raw")
#' # leer_casen("data-raw/2017_spss.rar")
#' @export
leer_casen <- function(archivo) {
  # checks ----
  stopifnot(is.character(archivo))
  message(glue::glue("Intentando leer el archivo {archivo}..."))
  if (!fs::file_exists(archivo)) {
    stop("No se pudo encontrar el archivo especificado.")
  }
  ext <- fs::path_ext(archivo)
  compressed_formats <- c("zip", "rar")
  uncompressed_formats <- c("sav", "dta")

  # read ----
  if (any(ext %in% c(compressed_formats, uncompressed_formats)) != TRUE) {
    formats_string <- paste(c(compressed_formats, uncompressed_formats[length(uncompressed_formats) - 1]), collapse = ", ")
    formats_string <- paste(c(formats_string, uncompressed_formats[length(uncompressed_formats)]), collapse = " o ")
    stop(glue::glue("Los metadatos de {archivo} indica que este archivo no sirve. Asegurate de que el formato es {formats_string}."))
  }

  if (any(ext %in% compressed_formats)) {
    message(glue::glue("Los metadatos de {archivo} indican que el formato comprimido es adecuado, intentando leer..."))
    d <- try(haven::read_sav(casen::extract(archivo)))
  }

  if (any(ext %in% uncompressed_formats)) {
    message(glue::glue("Los metadatos de {archivo} indican que el formato no comprimido es adecuado, intentando leer..."))
    if (ext == uncompressed_formats[[1]]) {
      d <- haven::read_sav(archivo)
    }
    if (ext == uncompressed_formats[[2]]) {
      d <- haven::read_dta(archivo)
    }
  }

  if (any(class(d) %in% "data.frame")) {
    message(glue::glue("{archivo} se pudo leer como tibble :-)"))
    d <- janitor::clean_names(d)
    return(d)
  } else {
    stop(glue::glue("{archivo} no se pudo leer como tibble :-("))
  }
}
