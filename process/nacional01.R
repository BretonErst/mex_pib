###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Librerías
library(tidyverse)
library(ggtext)


# Source
suppressMessages(source("cleaned/cleaned_data_nal.R"))


# gráfica del PIB trimestral 1994 2022
df01 %>% 
  ggplot(aes(x = fecha, y = valor)) +
  geom_line(color = "darkred") +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 16),
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "darkgrey", 
                                        hjust = 0)) +
  labs(title = "Desempeño del PIB México",
       subtitle = "Valores constantes, precios de 2013",
       x = NULL,
       y = "Millones de pesos a precios de 2013",
       caption = "Fuente: Elaboración propia con datos del INEGI <br>
       Juan L. Bretón, PMP")


