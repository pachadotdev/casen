context("testthat.R")

test_that("modelo_lineal_generalizado works properly", {
  r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
  est <- modelo_lineal_generalizado(r14, "ytotcorh ~ comuna + sexo", "expc")
  expect_is(est$betas, "data.frame")
  expect_output(str(est$betas), "7 variables")
  expect_is(est$prediccion, "data.frame")
  expect_output(str(est$prediccion), "10 variables")
})
