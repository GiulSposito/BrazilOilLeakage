library(tidyverse)
library(maps)
library(ggthemes)
library(xlsx)
library(sp)

# plot brazil and mark a dot

ggplot() +
  borders("world", "brazil") +
  geom_point(aes(x=-35,y=-5), size=5, color="blue", alpha=.5)
  

# traduzindo as coordenadas importadas para "decimal"
translateCoord <- function(.coords){

  directions <- str_extract(.coords,"\\w$")
  
  numbers <- .coords %>% 
    str_extract_all("(\\d\\.*)+") %>% 
    purrr::map(~paste0(.x[1],"d",.x[2],"'",.x[3],"\""))
  
  numbers %>% 
    purrr::map2(directions, ~paste0(.x, .y)) %>% 
    purrr::map(char2dms) %>% 
    purrr::map(as.numeric) %>% 
    unlist() %>% 
    return()

}

# converting coordinates
plan <- read.xlsx("./data/test.xlsx",1)

plan <- plan %>% 
  mutate(
    lat.degree = translateCoord(Latitude),
    lon.degree = translateCoord(Longitude)
  )

plan$Status

plan %>% 
  ggplot(aes(x=lon.degree, y=lat.degree)) +
  borders("world", "brazil") +
  geom_point(aes(color=Status), alpha=.4, size=3) +
  scale_color_discrete(c("black","grey","white"))



