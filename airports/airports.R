##### PACKAGES 

library(tidyverse)
library(sf)
library(nngeo)
library(showtext)
library(ggtext)

#### DATA 

# Load 

airports <- read_csv("https://gist.githubusercontent.com/fletchjeff/b60f46ca3c1aa51e9aea9fd86d0ab433/raw/81ee446939110daf8b5c3a79425ccf1100070578/airports.csv") |> 
  mutate(row_num= row_number()) |>
  st_as_sf(coords =c("Longitude", "Latitude"))

# Wrangling

brasil_airports <- airports %>% 
  filter(Country == "Brazil")

nn <- st_nn(brasil_airports, brasil_airports, k = 10, progress = F)
airports_connexions <- st_connect(brasil_airports, brasil_airports, ids = nn, progress = F)

#### AESTHETICS

# Font 

font_add_google("Nunito", "nunito", regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)

theme_set(
  theme_minimal() + 
    theme(
      plot.title = element_text(family = "nunito", size = 20),
      plot.subtitle = element_text(family = "nunito", size = 14, margin = margin(b = .25, unit = "cm")),
      plot.caption = element_text(family = "nunito", size = 10, margin = margin(t = .5, unit = "cm")),
      panel.grid = element_blank(), 
      axis.text = element_blank(),
      plot.background = element_rect(fill = "#E8E8E8", color = NA), 
      plot.margin = margin(c(.75,.5,.75,.5), unit = "cm")
    )
)

#### PLOT 
p <- airports_connexions %>% 
   ggplot() + 
   geom_sf(size = .75) +
   labs(
     title = "AEROPORTOS DO BRASIL",
     subtitle = "Conectados por seus 10-nearest-neighboors", 
     caption = "Data: Jeff Fletcher | Author: Ana Bodevan @anabodevan "
   ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "nunito", size = 14, face = "bold"),
    plot.subtitle = element_text(family = "nunito", size = 8, margin = margin(b = .25, unit = "cm")),
    plot.caption = element_text(family = "nunito", size = 6, margin = margin(t = .5, unit = "cm")),
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    plot.background = element_rect(fill = "#fefefe", color = NA), 
    plot.margin = margin(c(.75,.5,.75,.5), unit = "cm")
  )

p

ggsave("aeroportosbr.png", p, width = 7, height = 5, dpi = 300)

