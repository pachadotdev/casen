context("testthat.R")

test_that("media_agrupada works properly", {
  r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
  est <- estadistica_descriptiva(r14, "ytotcorh", c("comuna", "sexo"), "expc", 0.7)
  expect_is(est$media, "data.frame")
  expect_output(str(est$media), "7 variables")
  expect_is(est$mediana, "data.frame")
  expect_output(str(est$mediana), "7 variables")
  expect_is(est$cuantiles, "data.frame")
  expect_output(str(est$cuantiles), "7 variables")
})
