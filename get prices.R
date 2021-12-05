library(rvest)
library(tidyverse)
library(here)
library(lubridate)
library(feather)

# lê arquivo com as urls
df1 <- read_feather(path = paste(here(), "/urls.feather", sep = ""))

# transforma arquivo em lista
df1 <- as.list(df1)

# função que vai iterar url por url criando uma dataframe para preços e nomes
produtos <- function(urls){

  merc <- c()
  prec <- c()
  
  {
    
  read_primato <- read_html(paste(urls))
  
  Sys.sleep(runif(1,0,0.5))
  
  mercadoria_pn <- read_primato %>%
    html_nodes("a.link-nome-produto")%>%
    html_text(trim = T)
  
  merc = append(merc, mercadoria_pn)
  
  preco_pn <- read_primato %>%
    html_nodes(".preco span") %>%
    html_text2()
  
    prec <- append(prec, preco_pn)
    
    
    Sys.sleep(runif(1,0,0.5))
    
   
    }
  
return(data.frame(prec,merc))
  
}

# chamar função para cada linha da dataframe
get_prod <- map(df1[[1]], safely(produtos))

getprod2 <- get_prod

getprod2 <- unlist(getprod2, recursive = F)

getprod2 <- map(getprod2, tibble(getprod2,"prec" = getprod2$result$prec, "merc" = getprod2$result$merc))




# transforma as n listas em tabelas
dfprods <- purrr::map_dfr(get_prod, as.data.frame)

# adiciona timestamp
dfprods <- dfprods%>%
  mutate("timestamp"  = lubridate::today())

# salva arquivo do dia
write_feather(dfprods, path = paste(here(),"/data/preços", today(), ".feather", sep = ""))




