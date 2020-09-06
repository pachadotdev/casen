library(dplyr)

finp <- list.files("docs/data-rds", full.names = T)

d <- purrr::map_df(
  finp,
  function(x) {
    d <- readRDS(x)
    
    if (any("rama" %in% colnames(d))) {
      d <- d %>% 
        select(rama) %>% 
        mutate(anio = x) %>% 
        distinct() %>% 
        armonizar_rama()
    }
    
    if (any("rama1" %in% colnames(d))) {
      d <- d %>% 
        select(rama1) %>% 
        mutate(anio = x) %>% 
        distinct() %>% 
        armonizar_rama()
    }
    
    return(d)
  }
)
