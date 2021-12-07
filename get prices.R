library(rvest)
library(tidyverse)
library(here)
library(lubridate)
library(rio)
library(glue)


###############################################################


# lê arquivo com as urls
df1 <- feather::read_feather(path = paste(here(), "/urls.feather", sep = ""))


# função que vai iterar url por url criando uma lista para preços e nomes
produtos <- function(urls){
  
  merc <- c() # prepara vetor de mercadorias
  prec <- c() # prepara vetor de preços
  
  read_primato <- read_html(paste(urls)) # Lê url da lista
    
    Sys.sleep(runif(1,0,0.5)) # espera um valor aleatorio entre 0 e 0,5 segundos
    
    mercadoria_pn <- read_primato %>% 
      html_nodes("a.link-nome-produto")%>%
      html_text(trim = T) # Puxa todos os nomes das mercadorias da pagina
    
    merc = append(merc, mercadoria_pn) # Adiciona os nomes no vetor
    
    preco_pn <- read_primato %>%
      html_nodes(".preco span") %>%
      html_text2() # Puxa todos os preços da pagina
    
    prec <- append(prec, preco_pn) # Adiciona os preços no vetor
    
    
    Sys.sleep(runif(1,0,0.5)) # Espera mais um tempo aleatorio entre 0 e 0,5
    
    

  
  return(data.frame(prec,merc)) # creio que parte do problema de listas dentro de listas
                                # é causado por isso daqui, lembrar de corrigir quando der tempo
  
}


# Chamar função para cada linha da dataframe

get_prod <- map(df1$`.x[[i]]`, safely(produtos)) # Creio que a coluna precisa ser nomeada dessa forma por
                                                  # se tratar de um elemento que era uma list


# Retira um nivel da lista
# Remove as mensagens de erro

get_prod <- get_prod %>%
  rlist::list.flatten()%>%
  rlist::list.remove(range = c("error.call", "error.message"))


# Reagrupa os elementos da lista

get_prod <- tapply(
  unlist(
    get_prod, use.names = FALSE
    ),
  rep(
    names(get_prod), lengths(get_prod)), FUN = c
  ) # note to self: descobrir como essa parte funciona


# Transforma cada elemento em um DF

df_prods <- purrr::map(get_prod, as.data.frame)


# Une os DF em um só

df_prods <- as_tibble(df_prods)


#faz o possível para transformar os dados em tidy

df_prods <- df_prods %>%
  rename(
    'mercadorias' = result.merc,
    'precos' = result.prec
  ) %>%     # Renomear as colunas
  mutate(
    precos = as.character(unlist(precos)),
    mercadorias = as.character(unlist(mercadorias)),
    "timestamp"  = lubridate::today()
  )%>%      # Transformar os elementos de lista para caracteres
  mutate(
    precos = str_replace_all(precos, ",", ".")
  )%>%      # Substituir "," por "." nos preços
  mutate(
    precos = parse_double(precos)
  )         # Finalmente passar os preços de string para double


# Gerar summario simples

summ <- summary(df_prods)
diag <- dlookr::diagnose(df_prods)

# salva arquivo do dia

export(summ, file = glue("{here()}/data/summary {today()}.csv"), format = "csv")

export(df_prods, file = glue("{here()}/data/precos {today()}.feather"), format = "feather")

export(diag, file = glue("{here()}/data/diag {today()}.csv"), format = "csv")


