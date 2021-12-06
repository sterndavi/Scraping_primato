library(rvest)
library(tidyverse)
library(here)
library(lubridate)
library(feather)

# lê arquivo com as urls
df1 <- read_feather(path = paste(here(), "/urls.feather", sep = ""))

# transforma arquivo em lista
# df1 <- as.list(df1)

# função que vai iterar url por url criando uma lista para preços e nomes
produtos <- function(urls){

  merc <- c()
  prec <- c()
  
  {read_primato <- read_html(paste(urls))
  
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
get_prod <- map(df1$`.x[[i]]`[1:3], safely(produtos))



# retira um nivel da lista
get_prod <- rlist::list.flatten(get_prod)

# remove as mensagens de erro
get_prod <- rlist::list.remove(get_prod, range = c("error.call", "error.message"))

# reagrupa os elementos da lista
get_prod <- tapply(unlist(get_prod, use.names = FALSE), rep(names(get_prod), lengths(get_prod)), FUN = c)

# transforma cada elemento em um DF
df_prods <- purrr::map(get_prod, as.data.frame)

# une os DF em um só
df_prods <- as_tibble(df_prods)

df_prods <- df_prods %>%
  rename(
    'mercadorias' = result.merc,
    'precos' = result.prec
    ) %>%
  mutate(
    precos = as.character(unlist(precos)),
    mercadorias = as.character(unlist(mercadorias)),
    "timestamp"  = lubridate::today()
    )



# salva arquivo do dia
write_feather(dfprods, path = paste(here(),"/data/preços", today(), ".feather", sep = ""))




