
library(tidyverse)
library(ggrepel)
library(estimatr)
library(sandwich)
options(scipen = 999)
theme_set(theme_bw(base_size = 18))

conso <- readRDS("consocensoservel.rds")
conso$deltakast <- conso$p_kast - conso$p_jara


# COMUNAS MÁS PARECIDAS
conso %>% 
  ggplot() +
  aes(x = p_jara,
      y = p_kast,
      color = p_escincompleta*100) +
  geom_jitter() +
  geom_text_repel(aes(label = nomcomuna), size = 3, color = "black") +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0, 1, .05)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, .05)) +
  geom_hline(yintercept = .5) +
  geom_vline(xintercept = .5) +
  scale_color_viridis_c() +
  theme(plot.caption = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  labs(x = "% Jara",
       y = "% Kast",
       title = "Comunas que votaron más parecido",
       color = "% adultos de \nescolaridad incompleta",
       caption = "Escolaridad incompleta responde a personas de 20 años o más con menos de 12 años de escolaridad\n@sientifiko1")

# ESCOLARIDAD INCOMPLETA Y MARGEN PRO KAST
conso %>% 
  ggplot() +
  aes(x = p_escincompleta,
      y = deltakast) +
  geom_jitter() +
  geom_text_repel(aes(label = nomcomuna), size = 3) +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0, 1, .05)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-1, 1, .05)) +
  geom_hline(yintercept = 0) +
  theme(plot.caption = element_text(size = 8)) +
  labs(x = "% adultos con escolaridad incompleta",
       y = "% Kast - % Jara",
       title = "Adultos con escolaridad incompleta y voto por la ultraderecha",
       caption = "Escolaridad incompleta responde a personas de 20 años o más con menos de 12 años de escolaridad\n@sientifiko1")


reg1 <- lm_robust(deltakast ~ p_escincompleta,
                  data = conso,
                  fixed_effects = regabrv,
                  se_type = "HC3")

summary(reg1)


# PROMEDIO ESCOLARIDAD ADULTA Y MARGEN PRO KAST
conso %>% 
  ggplot() +
  aes(x = avg_esc_adulta,
      y = deltakast) +
  geom_jitter() +
  geom_text_repel(aes(label = nomcomuna), size = 3) +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-1, 1, .05)) +
  scale_x_continuous(breaks = seq(5, 17, 1)) +
  geom_hline(yintercept = 0) +
  theme(plot.caption = element_text(size = 8)) +
  labs(x = "Mediana de escolaridad en adultos",
       y = "% Kast - % Jara",
       title = "Promedio de escolaridad adulta y voto por la ultraderecha",
       caption = "\n@sientifiko1")

reg2 <- lm_robust(deltakast ~ avg_esc_adulta,
                  data = conso,
                  fixed_effects = regabrv,
                  se_type = "HC3")

summary(reg2)


# RURALIDAD Y MARGEN PRO KAST
conso %>% 
  ggplot() +
  aes(x = p_ruraladulta,
      y = deltakast) +
  geom_jitter() +
  geom_text_repel(aes(label = nomcomuna), size = 3) +
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-1, 1, .05)) +
  scale_x_continuous(breaks = seq(0, 1, .05),
                     labels = scales::percent) +
  geom_hline(yintercept = 0) +
  theme(plot.caption = element_text(size = 8)) +
  labs(x = "% de población rural adulta",
       y = "% Kast - % Jara",
       title = "Ruralidad y voto por la ultraderecha",
       caption = "\n@sientifiko1")

reg3 <- lm_robust(deltakast ~ p_ruraladulta,
                  data = conso,
                  fixed_effects = regabrv,
                  se_type = "HC3")

summary(reg3)

reg4 <- lm_robust(deltakast ~ poly(p_ruraladulta, 2),
                  data = conso,
                  fixed_effects = regabrv,
                  se_type = "HC3")

summary(reg4)

-1.0384/(2 * -0.5862)

# MIGRACIÓN VS MARGEN PRO KAST
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


# VENEZOLANOS
conso %>% 
  ggplot() +
  aes(x = p_migrvenezuela2,
      y = deltakast) +
  geom_jitter() +
  geom_text_repel(aes(label = nomcomuna), size = 3) +
  geom_smooth(method = "lm", formula = "y~poly(x,2)") +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0, 1, .02)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-1, 1, .05)) +
  geom_hline(yintercept = 0) +
  theme(plot.caption = element_text(size = 8)) +
  labs(x = "% migrantes venezolanos",
       y = "% Kast - % Jara",
       title = "Población venezolana y voto por la ultraderecha",
       caption = "@sientifiko1")

conso %>% 
  ggplot() +
  aes(x = p_migranteadulta2,
      y = deltakast) +
  geom_jitter() +
  geom_text_repel(aes(label = nomcomuna), size = 3) +
  geom_smooth(method = "lm", formula = "y~poly(x,2)") +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0, 1, .02)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-1, 1, .05)) +
  geom_hline(yintercept = 0) +
  theme(plot.caption = element_text(size = 8)) +
  labs(x = "% migrantes venezolanos adultos sobre total adultos (>= 20 años)",
       y = "% Kast - % Jara",
       title = "Población venezolana adulta y voto por la ultraderecha",
       caption = "Personas nacidas en Venezuela que llegaron a vivir entre 2017 a 2024\n@sientifiko1")

reg5 <- lm_robust(deltakast ~ poly(p_migranteadulta2, 2),
                          data = conso,
                          fixed_effects = regabrv,
                          se_type = "HC3")

summary(reg5)

