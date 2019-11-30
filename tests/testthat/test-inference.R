context("testthat.R")

test_that("modelo_lineal_generalizado works properly", {
  r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))
  cd <- configuracion_disenio(r14, "ytotcorh", c("comuna", "sexo"), "expc")
  est <- modelo_lineal_generalizado(cd, "ytotcorh ~ comuna + sexo")
  expect_is(est, c("svyglm","glm","lm"))
  expect_output(str(est), "List of 33")
})
