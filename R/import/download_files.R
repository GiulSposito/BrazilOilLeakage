library(tidyverse)
library(rvest)
library(lubridate)
library(glue)

##### SCRAP #####

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
  mutate( type = ifelse(type=="XLS","XLSX", type)) %>% # one file with "xls" extension
  mutate( link = paste0(base_url, doc_links) ) %>% 
  mutate( date=dmy(date) ) %>%
  as_tibble()

##### FILE DOWNLOAD #####

# check existence of destination folders
if(!file.exists("./data")) dir.create("./data")
if(!file.exists("./data/raw")) dir.create("./data/raw")

# check the existence of a previous download
# if(!file.exists("./data/scrapped_doclist.rds")) 

saveRDS(doc_list, "./data/scrapped_doclist.rds")

# TODO: if there is a previous download this file must do incremental download

##### EXCEL DOWNLOAD ##### 

# download xlsx files
xlsx_filenames <- doc_list %>% 
  filter(type=="XLSX") %>% 
  select(date, link) %>% 
  arrange(date) %>% 
  split(1:nrow(.)) %>% 
  map_chr(function(.x){
    filename <- paste0("./data/raw/",as.character(.x$date[1]), ".xlsx")
    download.file(url=.x$link[1], destfile = filename, mode = "wb")
    return(filename)
  })

# save an excel index
xlsx_index <- doc_list %>% 
  filter( type=="XLSX" ) %>% 
  arrange(date) %>% 
  mutate( filename = xlsx_filenames )

# save it
saveRDS(xlsx_index, "./data/excel_file_index.rds")

##### PDF DOWNLOAD #####

# download the PDF files for days where there is no XLSX file.
pdf_filenames <- doc_list %>% 
  # get the PDFs
  filter(type=="PDF") %>% 
  # in dates with no XLSX equivalent
  anti_join(filter(doc_list, type=="XLSX"), by="date") %>% 
  arrange(date) %>% 
  select(date, link) %>% 
  split(1:nrow(.)) %>% 
  map_chr(function(.x){
    filename <- paste0("./data/raw/",as.character(.x$date[1]), ".pdf")
    download.file(url=.x$link[1], destfile = filename, mode = "wb")
    return(filename)
  })

# save an pdf file index
pdf_index <- doc_list %>% 
  # get the PDFs
  filter(type=="PDF") %>% 
  # in dates with no XLSX equivalent
  anti_join(filter(doc_list, type=="XLSX"), by="date") %>% 
  arrange(date) %>%
  mutate( filename = pdf_filenames )

# save it
saveRDS(pdf_index, "./data/pdf_file_index.rds")


