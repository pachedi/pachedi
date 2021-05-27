
LUGAR <- function(TIPO, NDP){
  
  LUGARSP <- TIPO %>%
    filter(N.DE.PROYECTO == NDP)
  
  LUGARSP1 <-  LUGARSP[!is.na(LUGARSP$`TIPO.DE.INSTITUCION,.ESPACIO.O.LUGAR`),]
  
  LUGARSP2 <- LUGARSP1 %>%
    fill(ID.DE.PROYECTO, .direction = "down") %>%
    fill(PROVINCIA, .direction = "down") %>%
    select(1, 2, 27, 24, 3, 4, 25, 28, 26, 6, 5, 7, 8) %>%
    mutate('MES DE PRIMERA ACREDITACION' = '')
  
  
  CODSP1 <- CODSP %>%
    filter(N.DE.PROYECTO == NDP) %>%
    fill(PROVINCIA, .direction = "down")
  
  CODSP2 <-  CODSP1[!is.na(CODSP1$`TIPO.DE.INSTITUCION,.ESPACIO.O.LUGAR`),]
  
  CODSP2 <- CODSP2 %>%
    rename(PROVINCIA1 = PROVINCIA,
           DEPTOPARTIDO = 'DEPARTAMENTO/PARTIDO') %>%
    select(17, 18, 19, 20,21, 22)
  
  LUGARSP1F <- bind_cols(LUGARSP2, CODSP2)
  
  LUGARSPFI1 <- LUGARSP1F %>%
    relocate(14, .after = 8) %>%
    fill(8, .direction = "down") %>%
    fill(9, .direction = "down") %>%
    fill(10, .direction = "down") %>%
    fill(11, .direction = "down") %>%
    fill(12, .direction = "down") %>%
    fill(13, .direction = "down") %>%
    fill(14, .direction = "down")
}



















