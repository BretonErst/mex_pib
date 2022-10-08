###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Libraries
library(tidyverse)
library(qdapTools)


## Data acquisition
df00 <- read_csv("rawdat/conjunto_de_datos_pibe_actividad_21np2020_p.csv")


glimpse(df00)


## Data cleaning
df01 <- df00 %>%
  separate_rows(Descriptores, sep = "\\|") %>%
  mutate(Descriptores = str_remove(Descriptores, "\\<C\\d\\>$")) %>% 
  mutate(Descriptores = str_replace(Descriptores, "^\\w\\.\\w+\\-+", "")) %>% 
  cbind(mtabulate(as.data.frame.matrix(t(df01$Descriptores)))) %>% 
  as_tibble()
  

df01 %>% 
  filter(`Valores constantes` == 1 &
           `Valor agregado bruto` == 1)











