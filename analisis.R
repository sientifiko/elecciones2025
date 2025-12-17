
library(tidyverse)
library(ggrepel)
theme_set(theme_bw(base_size = 18))

conso <- readRDS("consocensoservel.rds")
conso$deltakast <- conso$p_kast - conso$p_jara


conso %>% 
  ggplot() +
  aes(x = p_migranteadulta2,
      y = deltakast) +
  geom_jitter() +
  geom_text_repel(aes(label = nomcomuna), size = 3) +
  geom_smooth(method = "lm", formula = "y~poly(x,2)") +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0, 1, .05)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-1, 1, .05)) +
  geom_hline(yintercept = 0) +
  theme(plot.caption = element_text(size = 8)) +
  labs(x = "% migrantes adultos sobre total adultos",
       y = "% Kast - % Jara",
       title = "Población migrante y voto por la ultraderecha",
       caption = "Se considera migrante a personas nacidas fuera de Chile que llegaron a vivir entre 2017 a 2024\n@sientifiko1")
