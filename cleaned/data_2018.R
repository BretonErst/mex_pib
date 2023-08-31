###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


# Librerías
library(tidyverse)


# Adquisición de datos
url <- 
  "https://www.inegi.org.mx/contenidos/programas/pib/2018/tabulados/des/pibt_cte_valor.xlsx"


download.file(url, "file.xlsx")

df00 <- 
  readxl::read_xlsx(path = "file.xlsx", 
                    skip = 4,
                    trim_ws = TRUE)




reng_interes <- 
  c("Producto interno bruto, a precios de mercado",
    "Actividades primarias",
    "Actividades secundarias",
    "Actividades terciarias")


# Limpieza de datos
df01 <-   
  df00 |> 
  filter(Denominación %in% reng_interes) |> 
  select(-2) |> 
  pivot_longer(cols = 2:123, names_to = "col", values_to = "valor") |> 
  janitor::clean_names() |> 
  mutate(denominacion = if_else(denominacion == "Producto interno bruto, a precios de mercado",
                                "Producto interno bruto",
                                denominacion),
         valor = as.numeric(valor)) |> 
  nest(.by = denominacion)

  
# función para poner semestre y fecha
prep_data <- function(df){
  df |> 
    mutate(fecha_inicio = seq.Date(from = as.Date("1993-01-01"),
                                   along.with = valor,
                                   by = "quarter"),
           fecha_final = fecha_inicio + months(3) - days(1),
           trimestre = quarter(fecha_final, type = "year.quarter"))
}

# incorporación con data
df02 <- 
  df01 |> 
  mutate(fechas = map(data, prep_data)) |> 
  unnest(fechas) |> 
  select(-c(col, data))
  

# remover archivos
file.remove("file.xlsx")

rm(df00)

rm(df01)
  
rm(reng_interes)

rm(url)

rm(prep_data)  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
      




