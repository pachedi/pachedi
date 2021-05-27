MHM2 <- function(COD, PASO1, NDP){
  
  MHMSP <- COD %>%
    filter(N.DE.PROYECTO == NDP)
  
  MHMSP1 <-  MHMSP[!is.na(MHMSP$COSTO.UNITARIO.TOTAL),]
  
  MHMSP11 <- MHMSP1 %>%
    select(16, 1, 22,21, 12, 24,25, 26,27,28) %>%
    mutate(PLAN = 'APROBADO',
           PROGRAMA = 'POTENCIAR',
           'TIPO DE ORGANIZACION' = 'OG',
           'MES PRIMERA ACREDITACION' = '')
  
  PROYECTOSP1 <- PASO1 %>%
    filter(N.DE.PROYECTO == NDP)
  
  PROYECTOSP2 <- PROYECTOSP1 %>%
    select(28, 26,  6, 5, 7) %>%
    slice(1) %>%
    rename(NOMBRE.DE.LA.ORGANIZACION = 1)
  
  PROYECTOSP1F <- data.frame(MHMSP11, cbind(zoo(, 1:nrow(MHMSP11)), as.zoo(PROYECTOSP2))) %>%
    fill(TIPO.DE.PROYECTO, .direction = "down") %>%
    fill(TIPO.Y.EJE.DEL.PROGRAMA, .direction = "down") %>%
    fill(EJE, .direction = "down") %>%
    fill(N.de.Expediente, .direction = "down") %>%
    fill(NOMBRE.DE.LA.ORGANIZACION, .direction = "down") %>%
    fill(PROVINCIA, .direction = "down") %>%
    fill(NOMBRE.GENÉRICO.DEL.PROYECTO, .direction = "down") %>%
    fill('DEPARTAMENTO.PARTIDO', .direction =  "down")
  
  PROYECTOSP1F1 <- PROYECTOSP1F %>%
    select(1,2, 11, 12, 3, 4, 13, 15, 14,16,17, 18,19,5, 6,
           7,8,9,10) %>%
    rename('ID DE PROYECTO' = G1)
}