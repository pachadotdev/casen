.onLoad <- function(libname = find.package("casen"), pkgname = "casen") {
  options(survey.lonely.psu = "certainty")
}

.onUnload <- function(libpath) {
  library.dynam.unload("casen", libpath)
}
