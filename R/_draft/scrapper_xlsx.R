library(tidyverse)
library(rvest)
library(xlsx)

# scrap and build url table

base_url <- "http://www.ibama.gov.br"
page <- read_html("http://www.ibama.gov.br/manchasdeoleo-localidades-atingidas")

updates <- page %>% 
  html_table() %>% 
  .[[1]]

doc_links <- page %>%
  html_nodes(xpath = "//td/a") %>% 
  html_attr("href")

doc_list <- updates %>% 
  set_names(c("date","description","items")) %>% 
  mutate( type = str_extract(items, "^[\\w]+") ) %>% 
  mutate( link = paste0(base_url, doc_links) ) %>% 
  as_tibble()

## download xlsx files

xlsx_url <- doc_list %>% 
  filter(type=="XLSX") %>% 
  .[1,] %>% 
  pull(link)

download.file(url = xlsx_url, destfile = "./data/test.xlsx", mode="wb")
read.xlsx("./data/test.xlsx",1)


