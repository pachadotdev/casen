context("testthat.R")

test_that("modelo_lineal_generalizado works properly", {
  r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
  est <- modelo_lineal_generalizado(r14, variables_independientes = "sexo", peso = "expc")
  expect_is(est, "data.frame")
  expect_output(str(est), "7 variables")
})

test_that("modelo_lineal_generalizado_2 works properly", {
  r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
  est <- modelo_lineal_generalizado_2(r14, "ytotcorh ~ comuna + sexo", "expc")
  expect_is(est, "data.frame")
  expect_output(str(est), "7 variables")
})
