#' Descarga la encuesta CASEN del sitio web del MDS
#' 
#' @param anios si no se indica un anio, descarga todos los anios disponibles
#' @param carpeta si no se indica una carpeta, descarga en la carpeta de
#' trabajo
#' @importFrom utils download.file
#' @importFrom glue glue
#' 
#' @return los archivos comprimidos de la encuesta CASEN descargados desde el
#' sitio web del MDS
#' 
#' @examples
#' # descargar todas las encuestas disponibles
#' # descargar_casen()
#'
#' # descargar CASEN 2017 en carpeta especifica
#' # descargar_casen(2017, "data-raw")
#' 
#' @export
descargar_casen <- function(anios = NULL, carpeta = getwd()) {
  # checks ----
  stopifnot(is.numeric(anios) | is.null(anios))
  stopifnot(is.character(carpeta), length(carpeta) == 1)

  # download ----
  try(dir.create(carpeta))

  all_years <- c(seq(1990, 2000, 2), seq(2003, 2009, 3), seq(2011, 2017, 2))

  if (!is.null(anios) & any(anios %in% all_years) == FALSE) {
    stop("La encuesta CASEN esta disponible entre los anios 1990 y 2017")
  }
    
  if (is.null(anios)) {
    anios <- c(seq(1990, 2000, 2), seq(2003, 2009, 3), seq(2011, 2017, 2))
  }

  urls <- c(
    paste0(
      "http://observatorio.ministeriodesarrollosocial.gob.cl/casen/layout/doc/bases/Casen",
      seq(1990, 1994, 2),
      ".zip"
    ),
    "http://observatorio.ministeriodesarrollosocial.gob.cl/casen/layout/doc/bases/Casen1996.rar",
    paste0(
      "http://observatorio.ministeriodesarrollosocial.gob.cl/casen/layout/doc/bases/Casen",
      seq(1998, 2000, 2),
      ".zip"
    ),
    "http://observatorio.ministeriodesarrollosocial.gob.cl/casen/layout/doc/bases/Casen2003.zip",
    "http://observatorio.ministeriodesarrollosocial.gob.cl/casen/layout/doc/bases/casen2006.zip",
    "http://observatorio.ministeriodesarrollosocial.gob.cl/casen/layout/doc/bases/Casen2009spss.rar",
    "http://observatorio.ministeriodesarrollosocial.gob.cl/casen/layout/doc/bases/casen2011_octubre2011_enero2012_principal_08032013spss.rar",
    "http://observatorio.ministeriodesarrollosocial.gob.cl/documentos/CASEN_2013_MN_B_Principal_spss.rar",
    "http://observatorio.ministeriodesarrollosocial.gob.cl/casen-multidimensional/casen/docs/casen_2015_spss.rar",
    "http://observatorio.ministeriodesarrollosocial.gob.cl/casen-multidimensional/casen/docs/casen_2017_spss.rar"
  )

  links <- data.frame(
    year = all_years,
    url = urls,
    file = paste0(carpeta, "/", all_years, "_spss", gsub(".*\\.", "\\.", urls)),
    stringsAsFactors = FALSE
  )

  links <- links[links$year %in% anios, ]

  for (j in 1:nrow(links)) {
    u <- links$url[[j]]
    f <- links$file[[j]]
    y <- links$year[[j]]
    
    if (!file.exists(f)) {
      message(glue::glue("Intentando descargar CASEN {y}..."))
      try(utils::download.file(u, f, mode = "wb"))
    } else {
      message(glue::glue("CASEN {y} ya existe, se omite la descarga"))
    }
  }
}
