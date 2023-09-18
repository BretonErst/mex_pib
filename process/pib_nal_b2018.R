###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


# Librerías
library(tidyverse)
library(ggtext)
library(displayBreton)


# Adquisición de datos
suppressWarnings(source("cleaned/data_2018.R", echo = FALSE))


# Tendencia lineal
df_pib <- 
  df02 |> 
  filter(denominacion == "Producto interno bruto")


tendencia <- 
  df_pib |>
  filter(fecha_inicio < "2018-07-02") |> 
  lm(formula = valor ~ fecha_inicio, 
     data = _)
  
max_pred <- 
  df_pib |> 
  mutate(predi = predict(tendencia, newdata = df_pib)) |> 
  slice_max(order_by = fecha_final) |> 
  pull(predi)

actual <- 
  df_pib_escala |> 
  slice_max(order_by = fecha_final) |> 
  pull(valor)

dif_tend <- 
  (actual -max_pred) / max_pred

identical(max_pred * dif_tend + max_pred, actual)


# Visualización de la serie con tendencia
df_pib |> 
  mutate(predi = predict(tendencia, newdata = df_pib)) |>  
  ggplot(aes(x = trimestre, 
             y = valor)) +
  geom_line(aes(color = "pib"),
            alpha = 0.8,
            linewidth = 0.65) +
  geom_smooth(aes(y = predi, 
                  color = "tendencia"),
              linewidth = 0.35, 
              method = "lm") +
  # annotate(geom = "text",
  #          label = paste("Diferencia ", 
  #                        scales::percent(dif_tend, accuracy = 0.1)),
  #          x = 2018, 
  #          y = 25050000,
  #          family = "Encode Sans Condensed",
  #          color = "grey30",
  #          size = 3.0) +
  labs(title = "Desempeño Histórico de la Economía Mexicana",
       subtitle = "Producto Interno Bruto. La línea roja refleja la tendencia hasta la última elección presidencial.",
       x = "Año.Trimestre",
       y = "Miles de millones de pesos",
       caption = "Fuente: INEGI, 
           Producto interno bruto trimestral. Miles de millones de pesos a precios de 2018. 
           Series desestacionalizadas.<br>
           Modelaje y visualización: Juan L. Bretón, PMP | @jluisbreton" ) +
  theme_breton() +
  theme(legend.position = "none") +
  scale_color_manual(name = NULL,
                     aesthetics = "color",
                     values = c("steelblue", "darkred"),
                     labels = c("PIB trimestral", "Tendencia lineal")) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ",",
                                                    scale = 1 / 1e3)) +
  scale_x_continuous(breaks = seq(min(df_pib$trimestre),
                                  max(df_pib$trimestre),
                                  by = 5))
  
ggsave("figures/pib18_01.jpg", device = "jpeg", dpi = "retina")



# Escalamiento de la serie por sexenio
# Función de escalamiento a 0
escalar_serie <- function(df){
  fact <- df |> 
    slice_min(order_by = fecha_final) |> 
    pull(valor)
  
  (df$valor - fact) / (fact)
}


# Función para numerar trimestre
numerar_trimes <- function(df){
  seq_along(along.with = df$trimestre)
}


# integra presidente
df_pib_escala <- 
  df_pib |> 
  mutate(presidente = as_factor(case_when(
    fecha_final >= as_date("1994-12-01") & fecha_final < as_date("2000-12-01") ~ "Zedillo",
    fecha_final >= as_date("2000-12-01") & fecha_final < as_date("2006-12-01") ~ "Fox",
    fecha_final >= as_date("2006-12-01") & fecha_final < as_date("2012-12-01") ~ "Calderón",
    fecha_final >= as_date("2012-12-01") & fecha_final < as_date("2018-12-01") ~ "Peña",
    fecha_final >= as_date("2018-12-01") ~ "López",
    TRUE ~ "Salinas"
  ))) |> 
  filter(presidente != "Salinas") |> 
  nest(.by = presidente) |> 
  mutate(valor_escalado = map(data, escalar_serie),
         num_trimestre = map(data, numerar_trimes))  |> 
  unnest(everything()) |> 
  select(-c(denominacion, fecha_inicio)) |> 
  mutate(num_trimestre = num_trimestre - 1)


# Visualización escalada
df_pib_escala |> 
  ggplot(aes(x = num_trimestre, 
             y = valor_escalado, 
             color = presidente)) +
  geom_line(alpha = 0.75) +
  geom_hline(yintercept = 00,
             color = "darkgrey",
             alpha = 0.5) +
  labs(title = "Desempeño de la Economía Mexicana en cada Periodo Presidencial",
       subtitle = "Producto Interno Bruto escalado.",
       x = "Trimestre",
       y = "Índice 0 = Inicio de cada sexenio",
       caption = "Fuente: INEGI, 
           Producto interno bruto trimestral. Precios de 2018. 
           Series desestacionalizadas.<br>
           Modelaje y visualización: Juan L. Bretón, PMP | @jluisbreton" ) +
  theme_breton() +
  theme(legend.position = "top") +
  scale_color_manual(name = "Presidente",
                     values = c("#34495E", "#F39C12", "#16A085", 
                                "#8E44AD", "#C0392B")) +
  scale_x_continuous(breaks = seq(min(df_pib_escala$num_trimestre),
                                  max(df_pib_escala$num_trimestre)))

ggsave("figures/pib18_02.jpg", device = "jpeg", dpi = "retina")


df_pib_escala |> 
  filter(presidente == "López")


# Tasas de crecimiento 
# con año anterior
val_actual <- 
  df_pib_escala |> 
  slice_max(order_by = fecha_final) |> 
  pull(valor)

val_anio_ant <- 
  df_pib_escala |> 
  filter(fecha_final == max(fecha_final) - years(1)) |> 
  pull(valor)

tasa_anual <- 
  (val_actual - val_anio_ant) / val_anio_ant
  
tasa_anual |> 
  as_tibble() |> 
  knitr::kable(col.names = "Tasa Año Anterior",
               digits = 4)


# con trimestre anterior
val_trim_ant <- 
  df_pib_escala |> 
  filter(presidente == "López") |> 
  select(-presidente) |> 
  mutate(ante = lag(valor)) |> 
  slice_max(order_by = fecha_final) |> 
  pull(ante)

tasa_trim <- 
  (val_actual - val_trim_ant) / val_trim_ant

tasa_trim |> 
  as_tibble() |> 
  knitr::kable(col.names = "Tasa Trimestre Anterior",
               digits = 4)


# Tasa de crecimiento desde el inicio del sexenio
calcula_sexenio <- function(df){
  val_inic <- df |> 
    slice_min(order_by = num_trimestre) |> 
    pull(valor)
  
  val_fina <- df |> 
    slice_max(order_by = num_trimestre) |> 
    pull(valor)
  
  (val_fina - val_inic) / val_inic
  
}

df_tasa_sexenal <- 
  df_pib_escala |> 
  nest(.by = presidente) |> 
  mutate(tasa_sexenal = map(data, calcula_sexenio),
         periodos = map(data, nrow)) |> 
  unnest(c(tasa_sexenal, periodos)) |> 
  select(-data) |> 
  mutate(tasa_promedio = tasa_sexenal / (periodos / 4))

df_tasa_sexenal |> 
  ggplot(aes(x = presidente, y = tasa_promedio, color = presidente)) +
  geom_point(size = 3.5, 
             alpha = 0.75) +
  geom_segment(aes(x = presidente, xend = presidente, 
                   y = 0, yend = tasa_promedio),
               alpha = 0.4,
               linewidth = 1.3) +
  geom_text(aes(label = scales::percent(tasa_promedio, accuracy = 0.01)),
            family = "Encode Sans Condensed",
            size = 2.8,
            vjust = -1.00,
            hjust = -0.25,
            color = "grey10") +
  geom_hline(yintercept = mean(df_tasa_sexenal$tasa_promedio),
             color = "grey60", linewidth = 2.5, alpha = 0.25) +
  annotate(geom = "text",
           x = 0.65, 
           y = mean(df_tasa_sexenal$tasa_promedio),
           label = scales::percent(mean(df_tasa_sexenal$tasa_promedio),
                                   accuracy = 0.01),
           size = 2.5,
           color = "grey45") +
  labs(title = "Tasa Promedio de Crecimiento de la Economía",
       subtitle = "Por año durante el sexenio de cada Presidente.",
       x = NULL,
       y = "Tasa anual promedio",
       caption = "Fuente: INEGI, 
           Producto interno bruto trimestral. Precios de 2018. 
           Series desestacionalizadas.<br>
           Modelaje y visualización: Juan L. Bretón, PMP | @jluisbreton" ) +
  scale_color_manual(values = c("#34495E", "#F39C12", "#16A085", 
                                "#8E44AD", "#C0392B")) +
  theme_breton() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))


ggsave("figures/pib18_03.jpg", device = "jpeg", dpi = "retina")


df_tasa_sexenal |> 
  filter(presidente != "López") |> 
  summarize(media_medias = mean(tasa_promedio))








