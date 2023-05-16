###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Librerías
library(tidyverse)
library(ggtext)
library(lubridate)


## source
suppressWarnings(source("cleaned/data_ready_quarter_national.R"))


## modelo lineal
model_lineal <- 
  lm(valor ~ fecha_final, 
     data = df02 %>% 
       filter(fecha_final < "2018-10-02"))

## último periodo
df02 %>% 
  slice_max(order_by = fecha, n = 1) 

# cambio respecto a tendendcia
pct_1 <- 
  df02 %>% 
  mutate(predi = predict(model_lineal, 
                         newdata = .)) %>% 
  summarize(pct_perdida = (last(.) %>% pull(valor) - last(.) %>% pull(predi)) /
              last(.) %>% pull(predi)) %>% 
  pull()


## visualización previa
df02 %>% 
  mutate(predi = predict(model_lineal, 
                         newdata = .)) %>% 
  ggplot(aes(x = fecha_final, 
             y = valor)) +
  geom_rect(xmin = as_date("2018-07-02"),
            xmax = max(df02$fecha_final),
            ymin = -Inf,
            ymax = Inf, 
            fill = "#F7F0E0", 
            alpha = 0.2) +
  geom_line(color = "steelblue",
            alpha = 0.7,
            linewidth = 0.75) +
  geom_smooth(aes(y = predi), 
              se = FALSE, 
              color = "darkred",
              linewidth = 0.5) +
  annotate(geom = "text",
           label = "Administración de López Obrador",
           x = as_date("2019-03-10"),
           y = 1.2e7, 
           angle = 90,
           size = 3.6, 
           color = "grey45",
           family = "Encode Sans Condensed") +
  annotate(geom = "text",
           label = scales::percent(pct_1, accuracy = 0.1),
           x = as_date("2022-10-25"),
           y = 1.9e7, 
           angle = 0,
           size = 3.3, 
           color = "grey35",
           family = "Encode Sans Condensed") +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 16),
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.line = element_line(color = "darkgrey"),
        panel.grid = element_line(color = "grey95"),
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "darkgrey", 
                                        hjust = 0)) +
  labs(title = "Efecto de la Administración de López Obrador en la Economía",
       subtitle = "La línea roja indica la tendencia de la Economía Mexicana desde 1993 hasta la última elección presidencial.",
       x = NULL,
       y = "Miles de millones de pesos (precios de 2013)",
       caption = "Fuente: INEGI, 
           Producto Interno Bruto Trimestral, 
           series desestacionalizadas. Miles de millones de pesos a precios de 2013.<br>
           Visualización: Juan L. Bretón, PMP | @BretonPmp") +
  scale_x_date(date_labels = "%Y",
               breaks = seq.Date(from = min(df02$fecha_final),
                                 to = max(df02$fecha_final) + 1,
                                 by = "4 year")) +
  scale_y_continuous(limits = c(min(df02$valor) - 0.1e6,
                                max(df02$valor) + 1e6),
                     labels = scales::dollar_format(scale = 1/1e3))

ggsave("figures/lob_01.jpg", device = "jpeg", dpi = "retina")



## modelo lineal ultimos 10 años
model_lineal_2 <- 
  lm(valor ~ fecha_final, 
     data = df02 %>% 
       filter(fecha >= "2012-01-01" & fecha < "2018-10-02"))


# cambio respecto a tendendcia
pct_2 <- 
  df02 %>% 
  mutate(predi = predict(model_lineal_2, 
                         newdata = .)) %>% 
  summarize(pct_perdida = (last(.) %>% pull(valor) - last(.) %>% pull(predi)) /
              last(.) %>% pull(predi)) %>% 
  pull()



## visualización previa
df02 %>% 
  filter(fecha >= "2012-01-01") %>% 
  mutate(predi = predict(model_lineal_2, 
                         newdata = .)) %>% 
  ggplot(aes(x = fecha_final, 
             y = valor)) +
  geom_rect(xmin = as_date("2018-07-02"),
            xmax = max(df02$fecha_final),
            ymin = -Inf,
            ymax = Inf, 
            fill = "#F7F0E0", 
            alpha = 0.2) +
  geom_line(color = "steelblue",
            alpha = 0.7,
            linewidth = 0.75) +
  geom_smooth(aes(y = predi), 
              se = FALSE, 
              color = "darkred",
              linewidth = 0.5) +
  annotate(geom = "text",
           label = "Administración de López Obrador",
           x = as_date("2019-02-28"),
           y = 1.65e7 - 10e4, 
           angle = 90,
           size = 3.6, 
           color = "grey45",
           family = "Encode Sans Condensed") +
  annotate(geom = "text",
           label = scales::percent(pct_2, accuracy = 0.1),
           x = as_date("2022-09-25"),
           y = 1.925e7, 
           angle = 0,
           size = 3.3, 
           color = "grey35",
           family = "Encode Sans Condensed") +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 16),
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.line = element_line(color = "darkgrey"),
        panel.grid = element_line(color = "grey95"),
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "darkgrey", 
                                        hjust = 0)) +
  labs(title = "Efecto de la Administración de López Obrador en la Economía",
       subtitle = "La tendencia de crecimiento se aceleró después de la recuperación de la crisis de 2008.",
       x = NULL,
       y = "Miles de millones de pesos (precios de 2013)",
       caption = "Fuente: INEGI, 
           Producto Interno Bruto Trimestral., 
           series desestacionalizadas. Miles de millones de pesos a precios de 2013.<br>
           Visualización: Juan L. Bretón, PMP | @BretonPmp") +
  scale_x_date(date_labels = "%Y",
               breaks = seq.Date(from = as_date("2012-01-01"),
                                 to = max(df02$fecha_final) + 1,
                                 by = "1 year")) +
  scale_y_continuous(limits = c(1.49e7,
                                max(df02$valor) + 2e6),
                     labels = scales::dollar_format(scale = 1/1e3))

ggsave("figures/lob_02.jpg", device = "jpeg", dpi = "retina")











