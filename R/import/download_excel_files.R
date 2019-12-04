library(tidyverse)
library(rvest)
library(lubridate)
library(glue)

# scrap and build url table
base_url <- "http://www.ibama.gov.br"

# page with update table
page <- read_html("http://www.ibama.gov.br/manchasdeoleo-localidades-atingidas")

# get the table
updates <- page %>% 
  html_table() %>% 
  .[[1]]

# get the href links
doc_links <- page %>%
  html_nodes(xpath = "//td/a") %>% 
  html_attr("href")

# put then togheter
doc_list <- updates %>% 
  set_names(c("date","description","items")) %>% 
  mutate( type = str_extract(items, "^[\\w]+") ) %>% 
  mutate( link = paste0(base_url, doc_links) ) %>% 
  mutate(date=dmy(date)) %>%
  as_tibble()  

# download xlsx files
filenames <- doc_list %>% 
  filter(type=="XLSX") %>% 
  select(date, link) %>% 
  split(1:nrow(.)) %>% 
  map_chr(function(.x){
    filename <- paste0("./data/raw/",as.character(.x$date[1]), ".xlsx")
    download.file(url=.x$link[1], destfile = filename, mode = "wb")
    return(filename)
  })

# save an excel index
xlsx_index <- doc_list %>% 
  filter( type=="XLSX" ) %>% 
  mutate( filename = filenames )

# save it
saveRDS(xlsx_index, "./data/excel_index.rds")
