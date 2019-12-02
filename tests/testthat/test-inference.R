context("testthat.R")

test_that("modelo_lineal_generalizado works properly", {
  cd <- configuracion_disenio(casen_2017_los_rios, "ytotcorh", c("comuna", "sexo"), "expc")
  est <- modelo_lineal_generalizado(cd, "ytotcorh ~ comuna + sexo")
  expect_is(est, c("svyglm","glm","lm"))
  expect_output(str(est), "List of 33")
})
