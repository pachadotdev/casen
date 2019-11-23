context("testthat.R")

test_that("leer_casen reads example dataset correctly", {
  r14 <- leer_casen(system.file(package = "casen", "extdata", "casen_2017_los_rios.zip"))

  expect_is(r14, "data.frame")
  expect_output(str(r14), "7 variables")
})

test_that("leer_casen can't read non-existing file", {
  expect_error(
    leer_casen(system.file(package = "casen", "extdata", "fake_path.zip")),
    "No se pudo encontrar el archivo especificado."
  )
})
