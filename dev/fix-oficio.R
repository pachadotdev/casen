library(dplyr)

finp <- list.files("docs/data-rds", full.names = T)

d <- purrr::map_df(
  finp,
  function(x) {
    d <- readRDS(x)
    
    if (any("oficio" %in% colnames(d))) {
      d <- d %>% 
        select(oficio) %>% 
        mutate(anio = x) %>% 
        distinct() %>% 
        armonizar_oficios()
    }
    
    if (any("oficio1" %in% colnames(d))) {
      d <- d %>% 
        select(oficio1) %>% 
        mutate(anio = x) %>% 
        distinct() %>% 
        armonizar_oficios()
    }
    
    return(d)
  }
)
