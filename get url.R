library(rvest)
library(tidyverse)
library(here)
library(lubridate)
library(feather)


# carregar página com a listagem e os links de todos os setores do mercado

read_primato_set <- read_html(
  paste(
    "https://supermercado.primato.com.br/loja/departamentos/", sep = ""
  ))


# preparar vetor

setores <- c()


# extrair href dos setores
get_setores <- read_primato_set%>%
  html_nodes(".h4,h4 a")%>%
  html_attr("href")


setores <- append(setores, get_setores)


# transformar hrefs em urls

link_setores <- data.frame( set_link = paste(
  "https://supermercado.primato.com.br", setores, "/?page=", sep = ""
)
)


# função para descobrir a quantidade de paginas em cada setor,
# não encontrei solução que não envolve loop porque é necessario testar
# cada uma das paginas até "proximo" não aparecer no html

num_page <- function(url_set){
  
  #define pagina inicial
  
  i <- 1
  
  
  # lê url da pagina inicial
  
  read_primato <- read_html(
    paste(
      url_set, i, sep = "")
  )
  
  
  # checa a existencia dos botões de navegação
  
  t <- read_primato %>%
    html_nodes(".btn-sm")%>%
    html_text(trim = T)
  
  
  # prepara vetor para output
  
  urls <- c()
  
  
  # lê url, espera 0 a 3 segundos, checa se há "proxima",
  # registra o link usado na lista e repete até pagina final
  
  while(t[1] == "Próxima"){
    
    read_primato <- read_html(
      paste(
        url_set, i, sep = "")
    )
    
    Sys.sleep(sample(0:3))
    
    t <- read_primato %>%
      html_nodes(".btn-sm")%>%
      html_text(trim = T)
    
    link <- str_c(url_set, i, sep = "")
    
    urls <- append(urls, link)
    
    
    i <- i+1
  }
  
  # retorna a lista de urls acumuladas no loop
  
  return(urls)
}



get_pages <- purrr::map(link_setores[1:34,], num_page)


df <- purrr::map_dfr(get_pages, as.data.frame)

write_feather(df, path = paste(here(), "/urls.feather", sep = ""))





