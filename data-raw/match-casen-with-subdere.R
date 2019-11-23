if (!require(pacman))
  install.packages("pacman")
p_load(data.table, readxl, purrr, dplyr, tidyr, stringr, haven, sjlabelled, janitor)

# codigos subdere ---------------------------------------------------------

territorial_codes_url <- "http://www.subdere.gov.cl/sites/default/files/documentos/cut_2018_v03.xls"
territorial_codes_xls <- "data-raw/unique_territorial_codes_subdere_sep_06_2018.xls"

if (!file.exists(territorial_codes_xls)) {
  download.file(territorial_codes_url, territorial_codes_xls)
}

communes <- read_excel("data-raw/unique_territorial_codes_subdere_sep_06_2018.xls") %>%
  clean_names()

communes2 <- communes %>%
  select(codigo_comuna_2017, nombre_comuna) %>%
  mutate(
    nombre_comuna = str_to_lower(nombre_comuna),
    nombre_comuna = iconv(nombre_comuna, from = "", to = "ASCII//TRANSLIT", sub = "")
  )

# extraer codigos ---------------------------------------------------------

casen_compressed <- list.files(path = "data-raw", pattern = ".rar|.zip", recursive = T, full.names = T)

for (j in 1:length(casen_compressed)) {
  try(
    system(paste("7z e -aos", casen_compressed[[j]], "-oc:data-raw/"))
  )
}

rm(casen_compressed)

casen_sav <- list.files(path = "data-raw", pattern = "sav", recursive = T, full.names = F)

for (j in 1:length(casen_sav)) {
  try(file.rename(paste0("data-raw/",casen_sav[[j]]), paste0("data-raw/", gsub(" ", "", casen_sav[[j]]))))
  try(file.rename(paste0("data-raw/",casen_sav[[j]]), paste0("data-raw/", gsub("_", "", casen_sav[[j]]))))
}

casen_sav <- list.files(path = "data-raw", pattern = "sav", recursive = T, full.names = T)

years <-  c(seq(1990, 2000, 2), seq(2003, 2009, 3), seq(2011, 2017, 2))

for (t in 1:7) {
  raw <- read_sav(casen_sav[[t]])

  raw2 <- raw %>%
    select(matches("comu")) %>%
    setNames(., "comu") %>%
    distinct() %>%
    mutate(comu2 = as_character(comu),
           comu2 = iconv(str_to_lower(comu2), from = "", to = "ASCII//TRANSLIT", sub = ""),
           comu = as.integer(comu))

  assign(paste0("communes_",years[[t]]), raw2)

  rm(raw,raw2)
}

communes90_03 <- mget(ls(pattern = "communes_")) %>%
  bind_rows() %>%
  arrange(comu2) %>%
  distinct(comu, .keep_all = T)

rm(list = ls(pattern = "communes_"))

for (t in 8) {
  raw <- read_sav(casen_sav[[t]])
  assign(paste0("casen_",years[[t]]), raw)

  raw2 <- raw %>%
    select(matches("COMU")) %>%
    setNames(., "comu") %>%
    distinct() %>%
    mutate(comu2 = as_character(comu),
           comu2 = iconv(str_to_lower(comu2), from = "", to = "ASCII//TRANSLIT", sub = ""),
           comu = as.integer(comu))
  assign(paste0("communes_",years[[t]]), raw2)

  rm(raw,raw2)
}

communes06 <- mget(ls(pattern = "communes_")) %>%
  bind_rows() %>%
  distinct(comu, .keep_all = T)

rm(list = ls(pattern = "communes_"))

for (t in 9) {
  raw <- read_sav(casen_sav[[t]])

  raw2 <- raw %>%
    select(matches("COMU")) %>%
    setNames(., "comu") %>%
    distinct() %>%
    mutate(comu2 = as_character(comu),
           comu2 = iconv(str_to_lower(comu2), from = "", to = "ASCII//TRANSLIT", sub = ""),
           comu = as.integer(comu))

  assign(paste0("communes_",years[[t]]), raw2)

  rm(raw,raw2)
}

communes09 <- mget(ls(pattern = "communes_")) %>%
  bind_rows() %>%
  distinct(comu, .keep_all = T)

rm(list = ls(pattern = "communes_"))

for (t in 10:12) {
  raw <- read_sav(casen_sav[[t]])

  raw2 <- raw %>%
    select(matches("COMU")) %>%
    setNames(., "comu") %>%
    distinct() %>%
    mutate(comu2 = as_character(comu),
           comu2 = iconv(str_to_lower(comu2), from = "", to = "ASCII//TRANSLIT", sub = ""),
           comu = as.integer(comu))

  assign(paste0("communes_",years[[t]]), raw2)

  rm(raw,raw2)
}

communes11_15 <- mget(ls(pattern = "communes_")) %>%
  bind_rows() %>%
  distinct(comu, .keep_all = T)

rm(list = ls(pattern = "communes_"))

for (t in 13) {
  raw <- read_sav(casen_sav[[t]])
  
  raw2 <- raw %>%
    select(matches("COMU")) %>%
    setNames(., "comu") %>%
    distinct() %>%
    mutate(comu2 = as_character(comu),
           comu2 = iconv(str_to_lower(comu2), from = "", to = "ASCII//TRANSLIT", sub = ""),
           comu = as.integer(comu))
  
  assign(paste0("communes_",years[[t]]), raw2)
  
  rm(raw,raw2)
}

communes17 <- mget(ls(pattern = "communes_")) %>%
  bind_rows() %>%
  distinct(comu, .keep_all = T)

rm(list = ls(pattern = "communes_"))

# unir codigos ------------------------------------------------------------

communes90_03 <- communes90_03 %>% 
  mutate(
    comu2 = case_when(
      comu2 == "paihuano" ~ "paiguano",
      comu2 == "imperial" ~ "nueva imperial",
      comu2 == "trehuaco" ~ "treguaco",
      comu2 == "teodoro schimdt" ~ "teodoro schmidt",
      comu2 == "coyhaique" ~ "coihaique",
      comu2 == "puerto aisen" ~ "aisen",
      TRUE ~ comu2
    )
  ) %>% 
  left_join(communes2, by = c("comu2" = 'nombre_comuna')) %>% 
  # filter(is.na(codigo_comuna_2017)) %>% 
  select(-comu2)

communes06 <- communes06 %>% 
  mutate(
    comu2 = case_when(
      comu2 == "paihuano" ~ "paiguano",
      comu2 == "la calera" ~ "calera",
      comu2 == "llay llay" ~ "llaillay",
      comu2 == "alto bio bio" ~ "alto biobio",
      comu2 == "trehuaco" ~ "treguaco",
      comu2 == "teodoro schimdt" ~ "teodoro schmidt",
      comu2 == "coyhaique" ~ "coihaique",
      TRUE ~ comu2
    )
  ) %>% 
  left_join(communes2, by = c("comu2" = 'nombre_comuna')) %>% 
  # filter(is.na(codigo_comuna_2017)) %>% 
  select(-comu2)

communes09 <- communes09 %>% 
  mutate(
    comu2 = case_when(
      comu2 == "paihuano" ~ "paiguano",
      comu2 == "la calera" ~ "calera",
      comu2 == "llay llay" ~ "llaillay",
      comu2 == "alto bio bio" ~ "alto biobio",
      comu2 == "trehuaco" ~ "treguaco",
      comu2 == "teodoro schimdt" ~ "teodoro schmidt",
      comu2 == "coyhaique" ~ "coihaique",
      comu2 == "aysen" ~ "aisen",
      TRUE ~ comu2
    )
  ) %>% 
  left_join(communes2, by = c("comu2" = 'nombre_comuna')) %>% 
  # filter(is.na(codigo_comuna_2017)) %>% 
  select(-comu2)

communes11_15 <- communes11_15 %>% 
  mutate(
    comu2 = case_when(
      comu2 == "paihuano" ~ "paiguano",
      comu2 == "la calera" ~ "calera",
      comu2 == "llay llay" ~ "llaillay",
      comu2 == "alto bio bio" ~ "alto biobio",
      comu2 == "trehuaco" ~ "treguaco",
      comu2 == "teodoro schimdt" ~ "teodoro schmidt",
      comu2 == "coyhaique" ~ "coihaique",
      comu2 == "aysen" ~ "aisen",
      TRUE ~ comu2
    )
  ) %>% 
  left_join(communes2, by = c("comu2" = 'nombre_comuna')) %>% 
  # filter(is.na(codigo_comuna_2017)) %>% 
  select(-comu2)

communes17 <- communes17 %>% 
  mutate(
    comu2 = case_when(
      comu2 == "paihuano" ~ "paiguano",
      comu2 == "la calera" ~ "calera",
      comu2 == "llay llay" ~ "llaillay",
      comu2 == "alto bio bio" ~ "alto biobio",
      comu2 == "trehuaco" ~ "treguaco",
      comu2 == "teodoro schimdt" ~ "teodoro schmidt",
      comu2 == "coyhaique" ~ "coihaique",
      comu2 == "aysen" ~ "aisen",
      TRUE ~ comu2
    )
  ) %>% 
  left_join(communes2, by = c("comu2" = 'nombre_comuna')) %>% 
  # filter(is.na(codigo_comuna_2017)) %>% 
  select(-comu2)

# agregar periodos --------------------------------------------------------

communes90_03 <- communes90_03 %>% 
  mutate(
    valido_desde = 1990,
    valido_hasta = 2003
  ) %>% 
  select(starts_with("val"), codigo_casen = comu, codigo_subdere_2017 = codigo_comuna_2017)

communes06 <- communes06 %>% 
  mutate(
    valido_desde = 2006,
    valido_hasta = 2006
  ) %>% 
  select(starts_with("val"), codigo_casen = comu, codigo_subdere_2017 = codigo_comuna_2017)

communes09 <- communes09 %>% 
  mutate(
    valido_desde = 2009,
    valido_hasta = 2009
  ) %>% 
  select(starts_with("val"), codigo_casen = comu, codigo_subdere_2017 = codigo_comuna_2017)

communes11_15 <- communes11_15 %>% 
  mutate(
    valido_desde = 2011,
    valido_hasta = 2015
  ) %>% 
  select(starts_with("val"), codigo_casen = comu, codigo_subdere_2017 = codigo_comuna_2017)

communes17 <- communes17 %>% 
  mutate(
    valido_desde = 2017,
    valido_hasta = 2017
  ) %>% 
  select(starts_with("val"), codigo_casen = comu, codigo_subdere_2017 = codigo_comuna_2017)

codigos_casen <- list(
  communes90_03 = communes90_03,
  communes06 = communes06,
  communes09 = communes09,
  communes11_15 = communes11_15,
  communes17 = communes17
)

codigos_casen <- bind_rows(codigos_casen)
  
codigos_casen <- codigos_casen %>% 
  left_join(communes2, by = c("codigo_subdere_2017" = "codigo_comuna_2017")) %>% 
  arrange(nombre_comuna)

codigos_casen <- codigos_casen %>% 
  group_by(codigo_casen, codigo_subdere_2017, nombre_comuna) %>% 
  summarise(
    valido_desde = min(valido_desde),
    valido_hasta = max(valido_hasta)
  ) %>% 
  arrange(nombre_comuna, valido_desde) %>% 
  select(starts_with("val"), everything())

codigos_casen <- codigos_casen %>% select(-nombre_comuna)

save(codigos_casen, file = "data/codigos_casen.rda", compress = "xz")
