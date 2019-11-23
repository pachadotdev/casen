context("testthat.R")

test_that("media_agrupada works properly", {
  r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
  est <- media_agrupada(r14, "ytotcorh", "comuna", "expc")
  expect_is(est, "data.frame")
  expect_output(str(est), "5 variables")
})

test_that("mediana_agrupada works properly", {
  r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
  est <- mediana_agrupada(r14, "ytotcorh", "comuna", "expc")
  expect_is(est, "data.frame")
  expect_output(str(est), "5 variables")
})

test_that("percentiles_agrupados works properly", {
  r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
  est <- percentiles_agrupados(r14, "ytotcorh", c(0.25,0.5,0.75), c("comuna","sexo"), "expc")
  expect_is(est, "data.frame")
  expect_output(str(est), "7 variables")
})
