context("testthat.R")

verify_output("agrupada.txt", {
  "# media_argrupada()"
  cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
  media_agrupada(cd)

  "# mediana_argrupada()"
  cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
  mediana_agrupada(cd)

  "# percentiles_agrupados()"
  cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
  percentiles_agrupados(cd, 0.7)
})
