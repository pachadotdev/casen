\name{codigos_casen}
\alias{codigos_casen}
\docType{data}
\title{
Codigos de comunas encuesta CASEN
}
\description{
Proporciona todos los codigos de comunas usados en la encuesta CASEN y registra
sus cambios en el tiempo.
}
\usage{codigos_casen}
\format{
  Un tibble con 683 filas y las siguientes cuatro columnas.
  \describe{
    \item{\code{valido_desde}}{primer anio en que se usa el codigo}
    \item{\code{valido_hasta}}{ultimo anio en que se usa el codigo}
    \item{\code{codigo_casen}}{codigo de comuna, valido solo en la encuesta}
    \item{\code{codigo_subdere_2017}}{codigo oficial de comuna, tiene validez legal}
  }
}
\source{
Elaboracion propia con base en datos de Ministerio de Desarrollo Social (MDS).
}
\examples{
codigos_casen
}
\keyword{datasets}
