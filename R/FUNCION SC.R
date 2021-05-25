library(tidyverse)
library(openxlsx)

options(scipen = 999)

rm(list = ls())

CODSC <- read.xlsx("C:/Users/Asus/Desktop/Articulacion social productiva/codificar/CODIFICAR/Cod Misiones/Plan de Actividad Misiones Potenciar Misiones.xlsx",
                   sheet = "2.2PLAN_DE_ACTIV_SOCIOCOMUNI")

TABLA <- read.xlsx("C:/Users/Asus/Desktop/Articulacion social productiva/codificar/CODIFICAR/Cod Misiones/INDEC Y TABLA.xlsx",
                   sheet = "TABLA")

INDEC <- read.xlsx("C:/Users/Asus/Desktop/Articulacion social productiva/codificar/CODIFICAR/Cod Misiones/INDEC Y TABLA.xlsx",
                   sheet = "INDEC")

EXP <- read.xlsx("C:/Users/Asus/Desktop/Articulacion social productiva/codificar/CODIFICAR/Cod Misiones/LISTADO DE UG CON EX Y CANTIDAD DE TIT_16_4_21 V2.xlsx")


EXP <- EXP %>% 
  filter(PROVINCIA == 'MISIONES')

EXP1 <- EXP %>% 
  select(3:4)

INDEC1 <- INDEC %>% 
  select(-(3:21)) %>% 
  mutate('PROV - DEP' = prov_depto) %>% 
  select(-(1))


CODIFICADO <- function(NPRO) {
  PRO1 <- CODSC %>% 
    filter(N.DE.PROYECTO == NPRO)
  
  PRO1 <- PRO1 %>% 
    unite('PROV - DEP', c(13,12), sep = " ", remove = FALSE, na.rm = TRUE)
  
  PRO1BB <- PRO1[-1,] %>% 
    mutate('N.de.Expediente' = '')
  
  PRO1B <- PRO1 %>% 
    left_join(., INDEC1, by = 'PROV - DEP') %>% 
    slice(1)
  
  TABLASC <- TABLA %>% 
    slice(8:14) %>% 
    select(19:20) %>% 
    rename(EJE = 'PROYECTOS.SOCIO-PRODUCTIVOS') %>% 
    rename(EJES = 'Columna21')
  
  PRO1B <- PRO1B %>% 
    left_join(., TABLASC, by = 'EJE') %>% 
    left_join(., EXP1, by = 'PROVINCIA') %>% 
    mutate(PROGRAMA = 2) %>% 
    mutate('TIPO DE ORGANIZACIÓN' = 2) %>% 
    mutate('TIPO PROYECTO' = 2) %>% 
    unite('ID.DE.PROYECTO', c(21,24,25,26,22,1), sep = "-", remove = FALSE, na.rm = TRUE)
  
  
  PRO1A <-   bind_rows(PRO1B, PRO1BB)
  
  PRO1A <- PRO1A %>% 
    mutate(G1 = ID.DE.PROYECTO,
           G2 = ID.DE.PROYECTO) %>% 
    fill(G1, .direction = "down") %>% 
    fill(G2, .direction = "down")
  
  PRO1AT <- PRO1A [, c(2, 25, 24, 22, 13, 1, 26, 27, 23,4,
                       3, 5, 6, 7, 8 , 28 , 9, 10, 11, 12, 
                       14, 15, 29, 16, 17, 18, 19, 20, 21)]
}

RESULTADO1 <-  CODIFICADO(1)
RESULTADO2 <-  CODIFICADO(2)
RESULTADO3 <-  CODIFICADO(3)
RESULTADO4 <-  CODIFICADO(4)
RESULTADO5 <-  CODIFICADO(5)
RESULTADO6 <-  CODIFICADO(6)
RESULTADO7 <-  CODIFICADO(7)





