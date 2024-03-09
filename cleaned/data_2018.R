###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################


## Librerías
library(tidyverse)


## Adquisición de datos
# url del archivo fuente
url <- 
  "https://www.inegi.org.mx/contenidos/programas/pib/2018/tabulados/des/pibt_cte_valor.xlsx"

# descarga del archivo
download.file(url, "file.xlsx")

# importación del archivo
df00 <- 
  readxl::read_xlsx(path = "file.xlsx", 
                    skip = 4,
                    trim_ws = TRUE)



## Limpieza de datos
# renglones de interés
reng_interes <- 
  c("Producto interno bruto, a precios de mercado",
    "Actividades primarias",
    "Actividades secundarias",
    "Actividades terciarias")


# conversión a formato tidy
df01 <-   
  df00 |> 
  filter(Denominación %in% reng_interes) |> 
  pivot_longer(cols = 3:ncol(df00), 
               names_to = "col", 
               values_to = "val") |> 
  janitor::clean_names() |> 
  mutate(denominacion = if_else(denominacion == "Producto interno bruto, a precios de mercado",
                                "Producto interno bruto",
                                denominacion),
         val = as.numeric(val)) |> 
  select(-2) |> 
  nest(.by = denominacion)

  
# función para poner semestre y fecha
prep_data <- function(df){
  df |> 
    mutate(fecha_inicio = seq.Date(from = as.Date("1993-01-01"),
                                   along.with = val,
                                   by = "quarter"),
           fecha_final = fecha_inicio + months(3) - days(1),
           trimestre = quarter(fecha_final, 
                               type = "year.quarter"))
}


# incorporación con data con semestre y fecha
df02 <- 
  df01 |> 
  mutate(fechas = map(data, prep_data)) |> 
  unnest(fechas) |> 
  select(-c(col, data)) |> 
  rename(valor = val)
  

## Labores finales
# remoción de archivos
file.remove("file.xlsx")

rm(df00)

rm(df01)
  
rm(reng_interes)

rm(url)

rm(prep_data)  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
      




