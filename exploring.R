library(tidyverse)
library(here)
library(glue)
library(DataExplorer)



df0712 <- feather::read_feather(path = glue("{here()}/data/precos 2021-12-07.feather"))
df0312 <- feather::read_feather(path = glue("{here()}/data/preços2021-12-03.feather"))
df0812 <- feather::read_feather(path = glue("{here()}/data/precos 2021-12-08.feather"))
df0912 <- feather::read_feather(path = glue("{here()}/data/precos 2021-12-09.feather"))
df1012 <- feather::read_feather(path = glue("{here()}/data/precos 2021-12-10.feather"))

df0312 <- df0312 %>%
  rename(
    'mercadorias' = merc,
    'precos' = prec
  ) %>%     # Renomear as colunas
  mutate(
    precos = as.character(unlist(precos)),
    mercadorias = as.character(unlist(mercadorias)),
    "timestamp"  = lubridate::ymd(20211203)
  )%>%      # Transformar os elementos de lista para caracteres
  mutate(
    precos = str_replace_all(precos, ",", ".")
  )%>%      # Substituir "," por "." nos preços
  mutate(
    precos = parse_double(precos)
  )         # Finalmente passar os preços de string para double

dfj <- df0312 %>%
  add_row(df0712)%>%
  add_row(df0812)%>%
  add_row(df0912)


create_report(dfj)

dataReporter::makeDataReport(dfj)

skimr::skim(dfj)


DataExplorer::create_report(df1012)
