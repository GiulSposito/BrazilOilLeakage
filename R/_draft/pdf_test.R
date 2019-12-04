# install.packages("tabulizer")
options(java.parameters = "-Xmx8000m")
library(tabulizer)
library(tidyverse)

out <- extract_tables("./data/2019-10-18_LOCALIDADES_AFETADAS_GERAL.pdf")
out <- extract_tables("./data/2019-09-02_a_2019-09-11_LOCALIDADES_AFETADAS.pdf")

str(out)
out[[2]]


pdf_table <- out[[2]]

manchas <- pdf_table[-1,] %>% 
  `colnames<-`(., pdf_table[1,]) %>% 
  as_tibble() %>% 
  mutate( Data_Avist = ymd(Data_Avist))

# 18-10-2019
manchas$Data_Avist %>% summary()
  
