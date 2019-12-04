library(tidyverse)
library(xlsx)
library(lubridate)

# traduzindo as coordenadas importadas para "decimal"
translateCoord <- function(.coords){
  
  # extrai a sigla de direcao (N,S,W,E)
  directions <- str_extract(.coords,"\\w$")
  
  # quebra os numeros e recompoe como xxDyy'zz"
  numbers <- .coords %>% 
    str_extract_all("(\\d\\.*)+") %>% 
    purrr::map(~paste0(.x[1],"d",.x[2],"'",.x[3],"\""))
  
  # adiciona direcao ao numero formatado e converte para decimal
  numbers %>% 
    purrr::map2(directions, ~paste0(.x, .y)) %>% 
    purrr::map(sp::char2dms) %>% 
    purrr::map(as.numeric) %>% 
    unlist() %>% 
    return()
  
}

# limpa e trata o status transformando em fator ordenave
statusToFactor <- function(.status){
  .status %>% 
    str_remove_all("Oleada - ") %>% 
    str_remove_all("[Ó|O]leo +") %>% 
    str_remove_all("[Ó|O]leo +") %>% 
    str_remove_all("- ") %>% 
    str_replace_all("Nao","Não") %>% 
    str_replace_all(" / ","/") %>% 
    str_replace_all("Vestigios","Vestígios") %>% 
    factor(labels = c("Não Observado", "Vestígios/Esparsos", "Manchas"), ordered = T) %>% 
    return()
}

# lendo arquivo de diritorio de excel
xlsx_index <- readRDS("./data/excel_index.rds")

# importa cada um dos excels (demora)
files <- xlsx_index %>%
  split(1:nrow(.)) %>% 
  map(function(.x){
    print(.x$filename)
    read.xlsx(file = .x$filename[1], sheetIndex = 1, colClasses="character", stringsAsFactors=F) %>% 
      as_tibble() %>% 
      mutate(file.date=.x$date[1])
  })

# salva a importacao para funcionar como cache 
saveRDS(files, "./data/imported_raw_excel.rds")

# lê o cache
files <- readRDS("./data/imported_raw_excel.rds")

# junta os excels in um unico dataframe
manchas <- files %>% 
  # o arquivo do dia 2019-11-06 nao tem coordenadas
  keep(function(.x){ .x$file.date[1] != "2019-11-06"} ) %>% 
  # para cada dataframe
  map_df(function(.x){
    
    # limpa os nomes das colunas
    col.names <- .x %>%
      names() %>% 
      tolower() %>%
      str_replace_all("cã.", "ci") %>% # acento no municipio 
      str_replace_all("name", "localidade") %>% 
      str_replace_all("latitutde","latitude")
    
    # novos nomes e colunas de interesse
    .x %>% 
      set_names(col.names) %>% 
      select(file.date, localidade, municipio, 
             estado, data_avist, data_revis,
             latitude, longitude, status) %>% 
      
      # trata pontuacao e tipagem
      mutate(
        localidade = iconv(localidade, from="UTF-8", to="LATIN1"),
        municipio  = iconv(municipio, from="UTF-8", to="LATIN1"),
        estado     = iconv(estado, from="UTF-8", to="LATIN1"),
        status     = iconv(status, from="UTF-8", to="LATIN1"),
        status     = statusToFactor(status),
        data_avist = ymd(as.character(data_avist)), 
        data_revis = ymd(as.character(data_revis))
      ) %>% 
      
      # retorna um dataset padronizado
      # para ser concatenado num so
      return()
  }) %>% 
  
  # converte cordenadas de string para decimal
  mutate(
    lat.degree = translateCoord(latitude),
    lon.degree = translateCoord(longitude)
  ) %>% 
  
  # trata casos em que a coordenada ja esta decimal
  mutate(
    lat.degree = ifelse(is.na(lat.degree), as.numeric(latitude), lat.degree),
    lon.degree = ifelse(is.na(lon.degree), as.numeric(longitude), lon.degree)
  )

# trata casos onde o estado esta incorreto
# (as vezes sigla as vezes nome)
# primeiro cria uma tabela de para estado/sigla
estado_uf <- files[[1]] %>% 
  mutate(
    estado = iconv(estado, from="UTF-8", to="LATIN1")
  ) %>% 
  select(estado, sigla_uf) %>% 
  distinct() %>% 
  arrange(estado)

# adiciona sigla com base no nome do estado
manchas_clean <- manchas %>% 
  left_join(estado_uf, by = "estado") %>% 
  # ha casos em que o campo estado tem sigla, entao uso o prorio
  mutate(sigla_uf = ifelse(is.na(sigla_uf),estado,sigla_uf)) %>% 
  # sete casos sem info de estado
  filter(!is.na(sigla_uf)) %>% 
  select(-estado) %>% 
  # recompoe o nome do estado
  inner_join(estado_uf, by="sigla_uf") %>% 
  # pega colunas de interesse e salva arquivo de trabalho
  select(file.date, data_avist, data_revis, sigla_uf, estado, municipio, localidade, lat.degree, lon.degree, status)

saveRDS(manchas_clean, "./data/manchas.rds")


