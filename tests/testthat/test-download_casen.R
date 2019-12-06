context("testthat.R")

test_that("descargar_casen_* downloads dataset correctly", {
  descargar_casen_mds(1990, "here")
  descargar_casen_github(1990, "here")
})

test_that("descargar_casen_* can't read non-existing file", {
  expect_error(
    descargar_casen_mds(1813, "here"),
    "La encuesta CASEN"
  )
  expect_error(
    descargar_casen_github(1813, "here"),
    "La encuesta CASEN"
  )
})
