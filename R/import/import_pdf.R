# we have to make a bigger heap
options(java.parameters = "-Xmx8000m")

library(tabulizer)
library(tidyverse)

# loading file index
pdf_file_index <- readRDS("./data/pdf_file_index.rds") %>% 
  arrange(date)

safeReadTables <- safely(function(.x){gc();print(.x);extract_tables(.x)})

# read tables
pdf_tables <- pdf_file_index$filename %>% 
  map(safeReadTables)

# save raw tables
saveRDS(pdf_tables,"./data/imported_raw_pdf.rds")

