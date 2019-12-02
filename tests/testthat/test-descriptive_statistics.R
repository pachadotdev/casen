context("testthat.R")

test_that("media_agrupada works properly", {
  cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
  est <- media_agrupada(cd)
  expect_is(est, "data.frame")
  expect_output(str(est), "7 variables")
})

test_that("mediana_agrupada works properly", {
  cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
  est <- mediana_agrupada(cd)
  expect_is(est, "data.frame")
  expect_output(str(est), "7 variables")
})

test_that("percentiles_agrupados works properly", {
  cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
  est <- percentiles_agrupados(cd, 0.7)
  expect_is(est, "data.frame")
  expect_output(str(est), "7 variables")
})
