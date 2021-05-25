
library(tidyverse)
library(openxlsx)

rm(list = ls())

options(scipen = 999)

PROYECTOSP  <-  read.xlsx("C:/Users/Asus/Desktop/Articulacion social productiva/codificar/CODIFICAR/Cod Misiones/PASO 1 SP.xlsx")

RESUMENCOSTO <- read.xlsx("../Desktop/Articulacion social productiva/codificar/CODIFICAR/POTENCIAR_PLAN DE ACTIVIDAD_PARANA_OG_CODF VF.xlsx",
                          sheet = "3.PLANILLA RESUMEN COSTO")

RUBRO <- function(PROYECTO, COSTO) {
  COSTO <- COSTO %>% 
    slice(3:9) %>% 
    select(2:3) %>% 
    pivot_wider(names_from = 'X2', values_from = 'X3')
  
  PROYECTO <- PROYECTO %>% 
    slice(1)
  
  PROYECTO <- PROYECTO %>%
    mutate('MES DE PRIMERA ACREDITACION' = '')
  
  PROYECTO <- PROYECTO %>% 
    select(1, 2, 27, 24, 3, 4, 25, 28, 30, 26, 6, 5, 7, 8)
  
  RUBROS <- bind_cols(PROYECTO, COSTO)
}

resultado <- RUBRO(PROYECTOSP, RESUMENCOSTO)






