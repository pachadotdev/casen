context("testthat.R")

test_that("descargar_casen downloads dataset correctly", {
  descargar_casen(1990, "here")
})

test_that("descargar_casen can't read non-existing file", {
  expect_error(
    descargar_casen(1813, "here"),
    "La encuesta CASEN"
  )
})
