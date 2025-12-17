

library(tidyverse)

censo <- readRDS("consocenso2024.rds")
servel <- read_rds("consoservel2025.rds")

# loccenso <- censo %>%
#   select(nomregion, nomcomuna) %>%
#   unique()

# locservel <- servel %>%
#   select(region, comuna) %>%
#   unique()

# write.csv2(loccenso, "loccenso.csv", row.names = F)
# write.csv2(locservel, "locservel.csv", row.names = F)

tempservel <- read.csv2("locservel_con_cut.csv")


servel2 <- servel %>% 
  left_join(tempservel, by = c("region", "comuna"))


conso <- censo %>% 
  left_join(servel2 %>% 
              select(3:12),
            by = c("comuna" = "cut_comuna"))


write_rds(conso, "consocensoservel.rds")









