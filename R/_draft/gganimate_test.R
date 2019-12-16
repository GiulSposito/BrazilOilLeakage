library(tidyverse)
library(gganimate)
library(gifski)
library(lubridate)
library(glue)
library(ggmap)

p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()


anim <- p + transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

plot(p)
anim

manchas <- readRDS("./data/manchas.rds")


anim <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point(aes(colour = Species), size = 4) + 
  transition_states(Species,
                    transition_length = .2,
                    state_length = .2)

anim + 
  enter_fade() + 
  exit_shrink()

p <- manchas %>% 
  filter(status!="Não Observado") %>% 
  mutate(data_avist=ymd(data_avist)) %>% 
  ggplot(aes(x=lon.degree, y=lat.degree, color=status)) +
  geom_point(size=4, alpha=.5) +
  scale_color_manual(values=c("darkgrey","black")) +
  theme_minimal()

anim <- p  +
  theme( legend.position = "bottom" ) +
  transition_time(data_avist) +
  labs(title="Avistamento de Óleo nas Costas Brasileiras",
       subtitle = "Data Avistamento:  {frame_time}") +
  enter_grow() +
  exit_shrink() +
  shadow_mark(past = TRUE)

anim


# map

mch <- manchas %>% 
  filter(status!="Não Observado") %>% 
  # filter(sigla_uf %in% c("SE")) %>% 
  # filter(lat.degree >= -15) %>% 
  mutate(data_avist=ymd(data_avist)) 

bbox <- make_bbox(lon = mch$lon.degree, lat=mch$lat.degree, f = .1)
gmap <- get_map(location=bbox, source="google", maptype="terrain")

p <- ggmap(gmap) +
  geom_point(data=mch, aes(x=lon.degree, y=lat.degree,
                           color=status, group=localidade),
             size=4, alpha=.5) +
  scale_color_manual(values=c("darkgrey","black")) +
  theme_void()

anim <- p  +
  theme( legend.position = "bottom" ) +
  transition_time(data_avist) +
  labs(title="Avistamento de Óleo na Costa do Brazil",
       subtitle = "Data Avistamento:  {frame_time} - Fonte: IBAMA") +
  shadow_wake(wake_length = .15)

anim

### Data revisao

manchas <- readRDS("./data/manchas.rds")

prim.avist <- manchas %>% 
  select(date = data_avist, lat.degree, lon.degree, status, localidade) %>% 
  distinct() %>% 
  filter(complete.cases(.))

mch <- manchas %>% 
  select(date = data_revis, lat.degree, lon.degree, status, localidade) %>% 
  mutate(date = ifelse(date>=ymd(20200101), date-years(1), date )) %>%
  mutate(date = as_date(date)) %>% 
  filter(complete.cases(.)) %>%
  bind_rows(prim.avist) %>% 
  distinct()

bbox <- make_bbox(lon = mch$lon.degree, lat=mch$lat.degree, f = .1)
gmap <- get_map(location=bbox, source="google", maptype="terrain")

p <- ggmap(gmap) +
  geom_point(data=filter(mch, status!="Não Observado"),
             aes(x=lon.degree, y=lat.degree,
                 color=status, group=localidade),
             size=3, alpha=.5) +
  scale_color_manual(values=c("gold","red")) +
  theme_void()

anim <- p  +
  theme( legend.position = "bottom" ) +
  transition_time(date) +
  labs(title="Avistamento de Óleo na Costa do Brasil",
       subtitle = "Data Avistamento:  {frame_time} - Fonte: IBAMA") +
  shadow_wake(wake_length = .15)

anim

last.mch <- mch %>% 
  group_by(localidade) %>% 
  filter(date==max(date)) %>% 
  arrange(localidade)

ggmap(gmap) +
  geom_point(data=last.mch,
             aes(x=lon.degree, y=lat.degree,
                 color=status, group=localidade),
             size=1, alpha=.5) +
  scale_color_manual(values=c("green", "gold","red")) +
  theme_void() +
  labs(title="Situação das Praias",
       subtitle = glue("Apurado entre {min.date} e {max.date} - Fonte: IBAMA",
                       min.date=min(last.mch$date), max.date=max(last.mch$date)))
  
