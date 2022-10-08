###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Librerías
library(tidyverse)
library(lubridate)
library(ggtext)


## Adquisición de datos
df00 <- read_csv("rawdat/conjunto_de_datos_pibt_pibt_cte2022_t2.csv")

glimpse(df00)

## Limpieza de datos
# Serie de PIB trimestral 1993 - 2022 
name_columns <- colnames(df00)

# Remover columnas con resumen
columns_to_keep <- str_which(name_columns, "\\d...\\|T\\d+")

# PIB trimestral
df01 <- df00 %>% 
  filter(Descriptores == "B.1bP---Producto interno bruto<C1>") %>% 
  select(descriptor = Descriptores,
         columns_to_keep) %>% 
  pivot_longer(cols = 2:121, 
               names_to = "periodo", 
               values_to = "valor") %>% 
  mutate(año = str_match(periodo, "\\d...") %>% .[,1],
         mes = case_when(str_detect(periodo, "\\d...\\|T1") ~ "01-01",
                         str_detect(periodo, "\\d...\\|T2") ~ "04-01",
                         str_detect(periodo, "\\d...\\|T3") ~ "07-01",
                         str_detect(periodo, "\\d...\\|T4") ~ "10-01"),
         fecha = ymd(str_c(año, "-", mes))) %>% 
  select(-c(año, mes))
  
rm(name_columns)

rm(columns_to_keep)

rm(df00)






