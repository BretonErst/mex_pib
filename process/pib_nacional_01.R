###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Librerías
library(tidyverse)
library(ggtext)


## source
suppressWarnings(source("cleaned/data_ready_quarter_national.R"))


## modelo lineal
model_lineal <- lm(valor ~ fecha, 
                   data = df02 %>% 
                     filter(fecha < "2018-07-02"))

## último periodo
df02 %>% 
  slice_max(order_by = fecha, n = 1) 


## visualización previa
df02 %>% 
  mutate(predi = predict(model_lineal, 
                         newdata = .)) %>% 
  ggplot(aes(x = fecha, 
             y = valor)) +
  geom_line(color = "steelblue",
            alpha = 0.7,
            linewidth = 0.6) +
  geom_smooth(aes(y = predi), 
              se = FALSE, 
              color = "darkred",
              linewidth = 0.5)


## función para añadir número de índice
pon_indice <- function(base){
  seq_along(along.with = base$trimestre)
}

## función para calcular valor escalado
calc_escala <- function(base){
  fact <- base %>% 
    slice_min(order_by = fecha) %>% 
    pull(valor)
  
  (base$valor - fact) / fact
}


## integrar presidente
df03 <- df02 %>% 
  mutate(presidente = as_factor(case_when(
    fecha >= as_date("1994-12-01") & fecha < as_date("2000-12-01") ~ "Zedillo",
    fecha >= as_date("2000-12-01") & fecha < as_date("2006-12-01") ~ "Fox",
    fecha >= as_date("2006-12-01") & fecha < as_date("2012-12-01") ~ "Calderón",
    fecha >= as_date("2012-12-01") & fecha < as_date("2018-12-01") ~ "Peña",
    fecha >= as_date("2018-12-01") ~ "López",
    TRUE ~ "Salinas"
  ))) %>%
  filter(presidente != "Salinas") %>%
  nest(data = -presidente)


df04 <- df03 %>% 
  mutate(trimestre_sexenio = map(data, pon_indice),
         escalado = map(data, calc_escala)) %>% 
  unnest(everything())


## visualización de valores escalados
df04 %>% 
  drop_na(valor) %>% 
  ggplot(aes(x = trimestre_sexenio, y = escalado, color = presidente)) +
  geom_line(alpha = 0.6) +
  scale_color_manual(name = "Presidente",
                     values = c("#34495E", "#F39C12", "#16A085", 
                                "#8E44AD", "#C0392B")) +
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
        legend.position = "top") +
  labs(title = "Desempeño de la Economía en cada Periodo Presidencial",
       subtitle = "Producto Interno Bruto, base 2013",
       x = "Trimestres del Sexenio",
       y = "Índice: 0 = día 1 de cada sexenio",
       caption = "Fuente: INEGI, 
         Producto Interno Bruto trimestral, Base 2013, 
         series desestacionalizadas. Último registro: 2T 2022.<br>
         Visualización: Juan L. Bretón, PMP | @BretonPmp" )



















