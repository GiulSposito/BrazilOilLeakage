library(tidyverse)
library(ggmap)
library(xlsx)

# read and register Google Map Key
config <- yaml::read_yaml("./config.yml")
register_google(key=config$google_map_key)

# traduzindo as coordenadas importadas para "decimal"
translateCoord <- function(.coords){
  
  directions <- str_extract(.coords,"\\w$")
  
  numbers <- .coords %>% 
    str_extract_all("(\\d\\.*)+") %>% 
    purrr::map(~paste0(.x[1],"d",.x[2],"'",.x[3],"\""))
  
  numbers %>% 
    purrr::map2(directions, ~paste0(.x, .y)) %>% 
    purrr::map(sp::char2dms) %>% 
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

manchas <- plan %>% 
    filter(estado=="Sergipe", Status!="Oleo Nao Observado") 

bbox <- make_bbox(lon = manchas$lon.degree, lat=manchas$lat.degree, f = .1)
gmap <- get_map(location=bbox, source="google", maptype="terrain")

ggmap(gmap) +
  geom_point(data=manchas, 
             aes(x=lon.degree, y=lat.degree, color=Status), size=4, alpha=.5) +
  scale_color_manual(values=c("#000000","#555555")) +
  theme_void() +
  theme( legend.position = "none")

manchas %>% View()
