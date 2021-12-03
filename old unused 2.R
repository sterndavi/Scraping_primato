library(rvest)
library(tidyverse)
library(here)
library(lubridate)






read_primato_t <- read_html(
    paste(
        "https://supermercado.primato.com.br/loja/departamentos/", sep = ""
    ))

set <- c()
setores2 <- read_primato_t%>%
    html_nodes(".h4,h4 a")%>%
    html_attr("href")
    set <- append(set, setores2)

{set_c <- str_remove_all(set, "/loja/")
set_c <- str_remove_all(set_c,"/")}
    
    
for(n in set_c){

#Definir pontos de partida
merc <- c()
prec <- c()
i = 1

#ler html inicial para checar condição
read_primato <- read_html(
    paste(
        "https://supermercado.primato.com.br/loja/", n, "/?page=", i, sep = ""
    ))

#checa a existencia do botao "Proxima", que indica se há ou não mais paginas
t <- read_primato %>%
    html_nodes(".btn-sm")%>%
    html_text(trim = T)


#enquanto houver proximas paginas, repetir o loop
while(t[1] == "Próxima"){

    read_primato <- read_html(
        paste(
            "https://supermercado.primato.com.br/loja/", n, "/?page=", i, sep = ""
        ))
    
    t <- read_primato %>%
        html_nodes(".btn-sm")%>%
        html_text(trim = T)
    
    mercadoria_pn <- read_primato %>%
        html_nodes("a.link-nome-produto")%>%
        html_text(trim = T)
    
    merc = append(merc, mercadoria_pn)
    
    
    preco_pn <- read_primato %>%
        html_nodes(".preco span")%>%
        html_text()
    
    prec <- append(prec, preco_pn)
    
    
    i <- i+1

Sys.sleep(3)



}



df <- data.frame("data" = paste(today()),"mercadoria" = merc, "preco" = prec)

write_csv(df,
          file = paste(
              here(),
              "/data/", n, ".csv", sep = ""), append = T)
Sys.sleep(5)
closeAllConnections()
rm(list=ls())
}          
          

