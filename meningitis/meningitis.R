#### PACKAGES ----

library(pacman)
pacman::p_load(
  tidyverse, ggrepel, ggtext, showtext, janitor, scales,
  ggbump, stringr, readr, glue
)

#### DATA ----

data <- read_csv('polio/cause_of_deaths.csv') |> 
  clean_names()

meningitis <- data %>% 
  select(country_territory, meningitis, year) %>%
  filter(year >= 1995, year <= 2015)

top10 <- meningitis %>%
  group_by(country_territory) %>%
  summarise(all_deaths_meningitis = sum(meningitis), .groups = "drop") %>%
  slice_max(order_by = all_deaths_meningitis, n = 10) %>%
  pull(country_territory)

meningitis <- meningitis %>%
  filter(country_territory %in% top10) %>%
  group_by(year) %>%
  mutate(rank = rank(meningitis, ties.method = "random")) %>%
  ungroup()

delta_values <- meningitis %>%
  filter(year %in% c(1995, 2015)) %>%
  select(-meningitis) %>%
  pivot_wider(
    id_cols = country_territory, 
    names_from = year, 
    values_from = rank, 
    names_prefix = 'year_'
  ) %>%
  mutate(max_dif = year_2015 - year_1995)

max_decrease <- meningitis %>%
  filter(country_territory == delta_values %>% slice_max(max_dif, n = 1) %>% pull(country_territory))

max_increase <- meningitis %>%
  filter(country_territory == delta_values %>% slice_min(max_dif, n = 1) %>% pull(country_territory))

meningitis_p <- meningitis %>%
  filter(!country_territory %in% c(max_decrease$country_territory, max_increase$country_territory)) %>%
  mutate(country_territory = if_else(country_territory == 'Democratic Republic of Congo', 'DR Congo', country_territory))

#### PLOT ----

# Fonts
font_add_google("Lato", "lato", regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)

# Base plot
ggplot() +
  ## Background lines
  geom_bump(
    data = meningitis_p,
    aes(x = year, y = rank, group = country_territory),
    linewidth = 0.7, color = 'grey85'
  ) +
  geom_point(
    data = meningitis_p %>% filter(year %in% c(1995, 2015)),
    aes(x = year, y = rank),
    size = 2, color = 'grey90'
  ) +
  
  ## Left labels
  geom_text(
    data = meningitis_p %>% filter(year == min(year)),
    aes(x = year - 0.3, y = rank, label = country_territory),
    size = 3, hjust = 1, family = 'lato', color = 'grey60'
  ) +
  
  ## Right labels
  geom_text(
    data = meningitis_p %>% filter(year == max(year)),
    aes(x = year + 0.3, y = rank, label = country_territory),
    size = 3, hjust = 0, family = 'lato', color = 'grey60'
  ) +
  
  ## Highlight Increase (red)
  geom_bump(
    data = max_increase, 
    aes(x = year, y = rank, group = country_territory), 
    linewidth = 1.2, color = '#EA6052'
  ) +
  geom_point(
    data = max_increase %>% filter(year %in% c(1995, 2015)),
    aes(x = year, y = rank), 
    size = 6, color = '#EA6052'
  ) +
  geom_point(
    data = max_increase %>% filter(year %in% c(1995, 2015)),
    aes(x = year, y = rank), 
    size = 4, color = 'white'
  ) +
  geom_text(
    data = max_increase %>% filter(year == min(year)),
    aes(x = year - 0.4, y = rank, label = country_territory),
    size = 3, hjust = 1, family = 'lato', color = '#EA6052', fontface = "bold"
  ) +
  geom_text(
    data = max_increase %>% filter(year == max(year)),
    aes(x = year + 0.4, y = rank, label = country_territory),
    size = 3, hjust = 0, family = 'lato', color = '#EA6052', fontface = "bold"
  ) +
  geom_text(
    data = max_increase %>% filter(year %in% c(1995, 2015)),
    aes(x = year, y = rank, label = rank),
    size = 2.8, family = 'lato', fontface = "bold", color = '#EA6052'
  ) +
  
  ## Highlight Decrease (blue)
  geom_bump(
    data = max_decrease, 
    aes(x = year, y = rank, group = country_territory), 
    linewidth = 1.2, color = '#18436D'
  ) +
  geom_point(
    data = max_decrease %>% filter(year %in% c(1995, 2015)),
    aes(x = year, y = rank), 
    size = 6, color = '#18436D'
  ) +
  geom_point(
    data = max_decrease %>% filter(year %in% c(1995, 2015)),
    aes(x = year, y = rank), 
    size = 4, color = 'white'
  ) +
  geom_text(
    data = max_decrease %>% filter(year == min(year)),
    aes(x = year - 0.4, y = rank, label = country_territory),
    size = 3, hjust = 1, family = 'lato', color = '#18436D', fontface = "bold"
  ) +
  geom_text(
    data = max_decrease %>% filter(year == max(year)),
    aes(x = year + 0.4, y = rank, label = country_territory),
    size = 3, hjust = 0, family = 'lato', color = '#18436D', fontface = "bold"
  ) +
  geom_text(
    data = max_decrease %>% filter(year %in% c(1995, 2015)),
    aes(x = year, y = rank, label = rank),
    size = 2.8, family = 'lato', fontface = "bold", color = '#18436D'
  ) +
  
  ## Theme and scales
  scale_y_reverse(
    breaks = seq(min(meningitis$rank), max(meningitis$rank), 1)
  ) +
  scale_x_continuous(
    breaks = seq(1995, 2015, 5),
    expand = expansion(mult = c(.05, .15))
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "grey30"),
    plot.background = element_blank(),
    plot.margin = margin(t = 30, r = 40, b = 30, l = 40)
  ) +
  labs(
    title = 'Twenty Years of Meningitis Deaths (1995–2015)',
    subtitle = 'Ranking of top 10 countries by total meningitis deaths',
    x = NULL,
    caption = "Data: Our World in Data\nAuthor: Ana Bodevan"
  )

ggsave(
  "meningitis_bump_chart.png",
  width = 12,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

