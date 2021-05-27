MHM1 <- function(PROV, TIPO){
  
  EXP1 <- EXP %>%
    filter(PROVINCIA == 'PROV')
  
  EXP2 <- EXP %>%
    select(3, 6)
  
  
  #### DESCOMBINADAS TODAS CON NUMERO
  
  TIPOPROYECTO <- TABLA %>%
    select(21:22) %>%
    slice(1:3) %>%
    rename(TIPO.PROYECTO = Columna11)
  
  
  PRSC1 <- TIPO %>%
    filter(N.DE.PROYECTO %in% 1:100)
  
  PRSC2 <- PRSC1 %>%
    select(6, 1, 22, 21, 3, 10, 12, 11, 13, 14, 15, 17, 18, 19,
           20,21,22, 24, 25, 26, 27 , 28, 29, 8)
  
  PRSC3 <- PRSC2 %>%
    mutate(PROGRAMA = 'POTENCIAR') %>%
    mutate('TIPO ORGANIZACION' = 'OG') %>%
    left_join(., TIPOPROYECTO, by = 'TIPO.PROYECTO') %>%
    mutate(N.de.Expediente = N.DE.EXPEDIENTE) %>%
    select(-(5)) %>%
    mutate(PLAN = 'APROBADO')
  
  
  PRSC4 <- PRSC3 %>%
    unite('TIPO Y EJE DEL PROGRAMA' , c(25, 6), sep = ' ',  remove = FALSE, na.rm = TRUE) %>%
    left_join(., EXP2, by = 'N.de.Expediente') %>%
    mutate('DEPTO/PARTIDO' = `DEPARTAMENTO/PARTIDO`)
  
  PASO1SC <- PRSC4[!is.na(PRSC4$COSTO.UNITARIO.TOTAL),]
  
}
