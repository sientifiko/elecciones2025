library(tidyverse)


conso <- readRDS("consocensoservel.rds")



conso %>% 
  ggplot() +
  aes(p_migranteadulta,
      p_kast) %>% 
  geom_jitter() +
  labs(x = "% migrantes adultos sobre total adultos",
       y = "% voto Kast")
