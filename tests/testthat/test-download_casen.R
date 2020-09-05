context("testthat.R")

test_that("descargar_casen_* downloads dataset correctly", {
  descargar_casen_mds(1990, tempdir())
  descargar_casen_github(1990, tempdir())
})

test_that("descargar_casen_* can't read non-existing file", {
  expect_error(
    expect_warning(
    descargar_casen_mds(1813, tempdir()),
    "La encuesta CASEN"
    )
  )
  expect_error(
    expect_warning(
    descargar_casen_github(1813, tempdir()),
    "La encuesta CASEN"
    )
  )
})
