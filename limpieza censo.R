
library(tidyverse)
library(duckdb)

#============= FUNCIONES ====================

abreviar_region <- function(region) {
      recode(region,
                "Metropolitana de Santiago" = "RM",
                "La Araucanía" = "Araucanía",
                "Aysén del General Carlos Ibáñez del Campo" = "Aysén",
                "Arica y Parinacota" = "Arica",
                "Libertador General Bernardo O'Higgins" = "O'Higgins",
                "Magallanes y de la Antártica Chilena" = "Magallanes",
                .default = region  # Mantiene los demás sin cambios
  )
}


# =============== PROCESANDO CENSO =================


con <- dbConnect(duckdb::duckdb())

totpop <- dbGetQuery(con, "
SELECT 
  region,
  comuna,
  count(*) pobtotal
FROM read_csv_auto('personas_censo2024.csv')
GROUP BY region, comuna
")

totpopadulta <- dbGetQuery(con, "
SELECT 
  region,
  comuna,
  count(*) pobadulta
FROM read_csv_auto('personas_censo2024.csv')
where edad >= 20
GROUP BY region, comuna
")

totmigrante <- dbGetQuery(con, "
SELECT 
  region,
  comuna,
  count(*) pobmigrante
FROM read_csv_auto('personas_censo2024.csv')
where p27_nacionalidad = 3
GROUP BY region, comuna
")

totmigranteadulta <- dbGetQuery(con, "
SELECT 
  region,
  comuna,
  count(*) pobmigranteadulta
FROM read_csv_auto('personas_censo2024.csv')
where p27_nacionalidad = 3
  and edad >= 20
GROUP BY region, comuna
")

totpea <- dbGetQuery(con, "
SELECT 
  region,
  comuna,
  count(*) pea
FROM read_csv_auto('personas_censo2024.csv')
where edad between 15 and 67
GROUP BY region, comuna
")

totempleo <- dbGetQuery(con, "
SELECT 
  region,
  comuna,
  count(*) ocupada
FROM read_csv_auto('personas_censo2024.csv')
where sit_fuerza_trabajo = 1
  and edad between 15 and 67
GROUP BY region, comuna
")


escolaridad <- dbGetQuery(con, "
SELECT 
  region,
  comuna,
  median(escolaridad) med_esc_adulta
FROM read_csv_auto('personas_censo2024.csv')
WHERE escolaridad != -99
  and edad >= 20
GROUP BY region, comuna
")


escolaridadincomp <- dbGetQuery(con, "
SELECT 
  region,
  comuna,
  count(*) pobesc_incompleta
FROM read_csv_auto('personas_censo2024.csv')
WHERE escolaridad between 0 and 11
  and edad >= 20
GROUP BY region, comuna
")

territorios <- readxl::read_excel("diccionario_variables_censo2024.xlsx",
                                  sheet = "codigos_territoriales")

colnames(territorios) <- c("codigo", "division", "nombre")

region <- territorios %>% 
  filter(division == "Región") %>% 
  rename(nomregion = "nombre") %>% 
  select(1,3) %>% 
  mutate(regabrv = abreviar_region(nomregion))

comuna <- territorios %>% 
  filter(division == "Comuna") %>% 
  rename(nomcomuna = "nombre") %>% 
  select(1,3)


# ================= CONSOLIDANDO ========================== 

conso <- totpop %>% 
  left_join(totpopadulta, by = c("region", "comuna")) %>% 
  left_join(totpea, by = c("region", "comuna")) %>% 
  left_join(totmigrante, by = c("region", "comuna")) %>% 
  left_join(totmigranteadulta, by = c("region", "comuna")) %>% 
  left_join(totempleo, by = c("region", "comuna")) %>% 
  left_join(escolaridad, by = c("region", "comuna")) %>% 
  left_join(escolaridadincomp, by = c("region", "comuna")) %>% 
  left_join(region, by = c("region"="codigo")) %>% 
  left_join(comuna, by = c("comuna"="codigo")) %>% 
  mutate(p_migrantetotal = pobmigrante/pobtotal,
         p_migranteadulta = pobmigranteadulta/pobadulta,
         p_desempleo = 1-(ocupada/pea),
         p_escincompleta = pobesc_incompleta/pobadulta)

write_rds(conso, "consocenso2024.rds")















  



