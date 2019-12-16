library(tidyverse)

manchas <- readRDS("./data/manchas.rds")


glimpse(manchas)

manchas %>% 
  filter(localidade=="Praia de Japaratinga") %>%
  count(lat.degree, sort=T)

manchas %>% 
  filter( round(lat.degree,6) == -9.082222) %>% 
  arrange(file.date) %>% 
  View()


primeiro.avistamento <- manchas %>% 
  filter(data_avist <= data_revis, status!="Não Observado") %>%
  group_by(lat.degree, lon.degree) %>% 
  select(-file.date, -data_revis) %>% 
  rename(data=data_avist)

manchas %>% 
  filter(data_avist == data_revis, status!="Não Observado") %>% 
  glimpse()



manchas %>% 
  select(-file.date) %>% 
  distinct() %>% 
  filter(localidade == "Mangue Seco") %>% 
  arrange(data_avist, data_revis) %>% 
  View()
