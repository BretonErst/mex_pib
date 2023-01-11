###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Librerías
library(tidyverse)
library(ggtext)
library(ggrepel)


# Source
suppressMessages(source("cleaned/cleaned_data_nal.R"))




# gráfica del PIB trimestral 1994 2022
df01 %>% 
  filter(fecha >= as_date("2012-01-01")) %>% 
  ggplot(aes(x = fecha, 
             y = valor)) +
  geom_line(color = "darkred") +
  geom_hline(yintercept = max(df01$valor, na.rm = TRUE),
             color = "darkgrey",
             lty = 3) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 16),
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "darkgrey", 
                                        hjust = 0)) +
  labs(title = "Desempeño del PIB México",
       subtitle = "Valores constantes, precios de 2013",
       x = NULL,
       y = "Millones de pesos a precios de 2013",
       caption = "Fuente: Elaboración propia con datos del INEGI <br>
       Juan L. Bretón, PMP")



# función para poner numero de índice
pon_num <- function(base) {
  seq_along(along.with = base$periodo)
}

# función para calcular valor escalado
calc_escala <- function(base) {
  fac <- base %>% 
    slice_min(order_by = fecha) %>% 
    pull(valor)
  
  (base$valor - fac) / fac
}


df02 <- df01 %>% 
  mutate(presidente = as_factor(case_when(
    fecha >= as_date("1995-01-01") & fecha < as_date("2001-01-01") ~ "Zedillo",
    fecha >= as_date("2001-01-01") & fecha < as_date("2007-01-01") ~ "Fox",
    fecha >= as_date("2007-01-01") & fecha < as_date("2013-01-01") ~ "Calderon",
    fecha >= as_date("2013-01-01") & fecha < as_date("2019-01-01") ~ "Peña",
    fecha >= as_date("2019-01-01") ~ "Lopez",
    TRUE ~ "Salinas"
  ))) %>%
  filter(presidente != "salinas") %>%
  select(-descriptor) %>% 
  nest(data = -presidente)
  


df03 <- df02 %>% 
  mutate(rela = map(data, pon_num),
         escalado = map(data, calc_escala)) %>% 
  unnest(everything())


# gráfica de desempeño por presidente
df03 %>%
  drop_na(valor) %>% 
  mutate(last_val = ifelse(rela == max(rela), 
                           as.character(presidente), 
                           NA)) %>%
  ggplot(aes(x = rela,
             y = escalado,
             color = presidente)) +
  geom_line(alpha = 0.6) +
  geom_text_repel(aes(label = last_val),
                  size = 3,
                  direction = "y",
                  hjust = -3,
                  segment.size = 1,
                  segment.alpha = 0.5,
                  nudge_y = -0.01,
                  segment.linetype = "dotted") +
  scale_x_continuous(limits = c(0, 27)) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.line = element_line(color = "darkgrey"),
        panel.grid = element_line(color = "grey95"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 16),
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "darkgrey", 
                                        hjust = 0),
        legend.position = "none") +
  labs(title = "Desempeño de la Economía en cada Periodo Presidencial",
       subtitle = "Valores constantes, precios de 2013",
       x = "Trimestre",
       y = "Índice: 0 = día 1 del primer enero del sexenio",
       caption = "Fuente: Elaboración propia con datos del INEGI <br>
       Juan L. Bretón, PMP")























