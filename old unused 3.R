
df1 <- read_feather(path = paste(here(), "/urls.feather", sep = ""))

df1 <- as.list(df1)

urls <- df1[3,]

produtos <- function(urls){
  {
  merc <- c()
  prec <- c()
  read_primato <- read_html(paste(urls))
  
  mercadoria_pn <- read_primato %>%
    html_nodes("a.link-nome-produto")%>%
    html_text(trim = T)
  
  merc = append(merc, mercadoria_pn)
  
  
  Sys.sleep(sample(1:3))
  preco_pn <- read_primato %>%
    html_nodes(".preco span") %>%
    html_text2()
  
    prec <- append(prec, preco_pn)
    
    
    Sys.sleep(sample(1:3))
  }
  return(data.frame(prec,merc))
  
}

get_prod <- map(df1[[1]], produtos)

dfprods <- purrr::map_dfr(get_prod, as.data.frame)

dfprods <- dfprods%>%
  mutate("timestamp"  = lubridate::today())


write_feather(dfprods, path = paste(here(),"/data/preços", today(), ".feather", sep = ""))





