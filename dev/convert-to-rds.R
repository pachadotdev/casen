# I had to repair 2013 file with winrar and create a zip

library(purrr)
library(stringr)
library(casen)

descargar_casen(carpeta = "data-raw")

periodos <- c(seq(1990, 2000, 2), seq(2003, 2009, 3), seq(2011, 2017, 2))

archivos <- list.files("data-raw", full.names = T, pattern = "zip|rar")

try(dir.create("docs/data-rds"))

map(
  periodos,
  function(p) {
    i <- str_subset(archivos, as.character(p))
    o <- sprintf("docs/data-rds/%s.rds", p)
    if(!file.exists(o)) {
      d <- leer_casen(i)
      saveRDS(d, o, compress = "xz")
    }
  }
)
