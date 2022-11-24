###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Librerías
library(tidyverse)
library(lubridate)


## adquisición de datos
url <- "https://www.inegi.org.mx/contenidos/temas/economia/pib/pibt/tabulados/des/pibt_cte_valor.xlsx"

download.file(url, "file.xlsx")

df00 <- readxl::read_xlsx("file.xlsx", 
                          skip = 4, 
                          trim_ws = TRUE)

glimpse(df00)


## limpieza de datos
df01 <- df00 %>% 
  select(-2) %>% 
  filter(is.na(Denominación) | 
           Denominación == "Producto interno bruto, a precios de mercado") %>% 
  drop_na(...4) %>% 
  as.matrix() %>% 
  t() %>% 
  .[-1, ] %>% 
  as_tibble(repair = .name_repair)


df02 <- df01 %>% 
  select(trimestre = V1,
         valor = V2) %>% 
  mutate(trimestre = as_factor(trimestre),
         valor = as.double(valor),
         fecha = seq.Date(from = as_date("1993-01-01"),
                        along.with = trimestre,
                        by = "quarter")) %>% 
  relocate(fecha, .before = everything())





















