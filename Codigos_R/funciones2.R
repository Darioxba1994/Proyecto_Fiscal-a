#### Archivo para correr la Carga Laboral
source("Lectura_tasaCL.R") # Este documento crea un excel "TASA_2.xlsx"

Tasa <- function(){
  return(list(Flagrantes= AÑOSi, No_Flagrantes =AÑOSi1, Archivados = Ind ))
}

source("Lectura_Datos.R") ## 2 minutos en leer bases de datos
source("funciones1.R") ## Funciones para resultados


ResProv <- function(Provincia){
  PROVINCIA <- Datos_de_p(DXSiaf,Provincia) # Elijo data de la Provincia en estudio.
  ## Datos para el mapa de la provincia.
  Frec <- Grafico_P(Provincia,PROVINCIA) ## Datos de frecuecnia en orde de la data para mapa
  cantones <- st_read("ecuador.json")
  PROVINCIA1 <- cantones[cantones$DPA_DESPRO == chartr("Ñ", "Ð", toupper(Provincia)),]
  Canton <- c(Frec$NOMBRES$Nom_Can)
  library(writexl)
  library(xlsx)
  ## Crear documento Excel
  write.xlsx(Frec$NOMBRES, file = paste(Provincia,"xlsx",sep="."), sheetName = "Frecuencia")
  
  for (c in Canton) {
    print(c)
    CANTON <- Datos_de_c(PROVINCIA,c)
    # 1.3.1 Obtener las series de tiempo para el análisis de pronósticos del Cantón.
    Tipos_d <- names(table(PROVINCIA$Delito))
    Series <- A15_20(CANTON,Tipos_d)
    
    # 1.3.2 Análisis y Pronostico de Series
    ## Asignacion de fiscalias y delitos segun catalogo
    Fiscalias <- Asig_Fisc(Asignacion)
    Ser <- data.frame(Series[1])
    Ser[,2:length(Ser)] <- as.numeric(unlist(Ser[,2:length(Ser)])) ## cambio a terminos numericos las series 
    names(Ser)[1] <- "Tipo de Delito"
    Resultados <- Analisis(Ser,Asignacion)
    carga <- c() ## CARGA ASIGNADA A LA FISCALÍA.
    rm(F1,F2,F3,F4,F5,F6,F7,F8,F9)
    rm(r1,r2,r3,r4,r5,r6,r7,r8,r9)
    #Fiscalia 1
    
    if (dim(Resultados$DELFISC1)[1]==0) {
      r1 <- c("Fiscalia 1", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC1)[1]>0 & dim(Resultados$DELFISC1)[1]< 3) {
      F1 <- Cluster3(Resultados$DELFISC1,carga)
      r1 <- c("Fiscalia 1" , trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC1)[1] >= 3 & dim(Resultados$DELFISC1)[1]< 8) {
      F1 <- Cluster2(Resultados$DELFISC1,carga)
      r1 <- c("Fiscalia 1" , trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC1)[1] >= 8) {
      F1 <- Cluster(Resultados$DELFISC1,carga)
      r1 <- c("Fiscalia 1" , trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 2
    
    if (dim(Resultados$DELFISC2)[1]==0) {
      r2 <- c("Fiscalia 2", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC2)[1]>0 & dim(Resultados$DELFISC2)[1]< 3) {
      F2 <- Cluster3(Resultados$DELFISC2,carga)
      r2 <- c("Fiscalia 2" , trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC2)[1] >= 3 & dim(Resultados$DELFISC2)[1]< 8) {
      F2 <- Cluster2(Resultados$DELFISC2,carga)
      r2 <- c("Fiscalia 2" , trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC2)[1] >= 8) {
      F2 <- Cluster(Resultados$DELFISC2,carga)
      r2 <- c("Fiscalia 2" , trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 3
    
    if (dim(Resultados$DELFISC3)[1]==0) {
      r3 <- c("Fiscalia 3", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC3)[1]>0 & dim(Resultados$DELFISC3)[1]< 3) {
      F3 <- Cluster3(Resultados$DELFISC3,carga)
      r3 <- c("Fiscalia 3" , trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC3)[1] >= 3 & dim(Resultados$DELFISC3)[1]< 8) {
      F3 <- Cluster2(Resultados$DELFISC3,carga)
      r3 <- c("Fiscalia 3" , trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC3)[1] >= 8) {
      F3 <- Cluster(Resultados$DELFISC3,carga)
      r3 <- c("Fiscalia 3" , trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 4
    
    if (dim(Resultados$DELFISC4)[1]==0) {
      r4 <- c("Fiscalia 4", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC4)[1]>0 & dim(Resultados$DELFISC4)[1]< 3) {
      F4 <- Cluster3(Resultados$DELFISC4,carga)
      r4 <- c("Fiscalia 4" , trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC4)[1] >= 3 & dim(Resultados$DELFISC4)[1]< 8) {
      F4 <- Cluster2(Resultados$DELFISC4,carga)
      r4 <- c("Fiscalia 4" , trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC4)[1] >= 8) {
      F4 <- Cluster(Resultados$DELFISC4,carga)
      r4 <- c("Fiscalia 4" , trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 5
    
    if (dim(Resultados$DELFISC5)[1]==0) {
      r5 <- c("Fiscalia 5", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC5)[1]>0 & dim(Resultados$DELFISC5)[1]< 3) {
      F5 <- Cluster3(Resultados$DELFISC5,carga)
      r5 <- c("Fiscalia 5" , trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC5)[1] >= 3 & dim(Resultados$DELFISC5)[1]< 8) {
      F5 <- Cluster2(Resultados$DELFISC5,carga)
      r5 <- c("Fiscalia 5" , trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC5)[1] >= 8) {
      F5 <- Cluster(Resultados$DELFISC5,carga)
      r5 <- c("Fiscalia 5" , trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 6
    
    if (dim(Resultados$DELFISC6)[1]==0) {
      r6 <- c("Fiscalia 6", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC6)[1]>0 & dim(Resultados$DELFISC6)[1]< 3) {
      F6 <- Cluster3(Resultados$DELFISC6,carga)
      r6 <- c("Fiscalia 6" , trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC6)[1] >= 3 & dim(Resultados$DELFISC6)[1]< 8) {
      F6 <- Cluster2(Resultados$DELFISC6,carga)
      r6 <- c("Fiscalia 6" , trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC6)[1] >= 8) {
      F6 <- Cluster(Resultados$DELFISC6,carga)
      r6 <- c("Fiscalia 6" , trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo3))))
    }
    
   #Fiscalia 7 
    
    if (dim(Resultados$DELFISC7)[1]==0) {
      r7 <- c("Fiscalia 7", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC7)[1]>0 & dim(Resultados$DELFISC7)[1]< 3) {
      F7 <- Cluster3(Resultados$DELFISC7,carga)
      r7 <- c("Fiscalia 7" , trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC7)[1] >= 3 & dim(Resultados$DELFISC7)[1]< 8) {
      F7 <- Cluster2(Resultados$DELFISC7,carga)
      r7 <- c("Fiscalia 7" , trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC7)[1] >= 8) {
      F7 <- Cluster(Resultados$DELFISC7,carga)
      r7 <- c("Fiscalia 7" , trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 8
    
    if (dim(Resultados$DELFISC8)[1]==0) {
      r8 <- c("Fiscalia 8", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC8)[1]>0 & dim(Resultados$DELFISC8)[1]< 3) {
      F8 <- Cluster3(Resultados$DELFISC8,carga)
      r8 <- c("Fiscalia 8" , trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC8)[1] >= 3 & dim(Resultados$DELFISC8)[1]< 8) {
      F8 <- Cluster2(Resultados$DELFISC8,carga)
      r8 <- c("Fiscalia 8" , trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC8)[1] >= 8) {
      F8 <- Cluster(Resultados$DELFISC8,carga)
      r8 <- c("Fiscalia 8" , trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 9
    
    if (dim(Resultados$DELFISC9)[1]==0) {
      r9 <- c("Fiscalia 9", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC9)[1]>0 & dim(Resultados$DELFISC9)[1]< 3) {
      F9 <- Cluster3(Resultados$DELFISC9,carga)
      r9 <- c("Fiscalia 9" , trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC9)[1] >= 3 & dim(Resultados$DELFISC9)[1]< 8) {
      F9 <- Cluster2(Resultados$DELFISC9,carga)
      r9 <- c("Fiscalia 9" , trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC9)[1] >= 8) {
      F9 <- Cluster(Resultados$DELFISC9,carga)
      r9 <- c("Fiscalia 9" , trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo3))))
    }
    
    
    Demanda <- rbind.data.frame(r1,r2,r3,r4,r5,r6,r7,r8,r9)
    Demanda[,2:length(Demanda)] <- as.numeric(unlist(Demanda[,2:length(Demanda)]))
    colnames(Demanda) <- c("Fiscalias", "Grupo1" , "Grupo2" , "Grupo3")
    
    write.xlsx(Demanda, file = paste(Provincia,"xlsx",sep="."), sheetName = c, append = T )
  }
  
  return(DEm =Demanda)
}

  

ResProv1 <- function(Provincia){
  PROVINCIA <- Datos_de_p(DXSiaf,Provincia) # Elijo data de la Provincia en estudio.
  ## Datos para el mapa de la provincia.
  Frec <- Grafico_P(Provincia,PROVINCIA) ## Datos de frecuecnia en orde de la data para mapa
  cantones <- st_read("ecuador.json")
  PROVINCIA1 <- cantones[cantones$DPA_DESPRO == chartr("Ñ", "Ð", toupper(Provincia)),]
  Canton <- c(Frec$NOMBRES$Nom_Can)
  library(writexl)
  library(xlsx)
  ## Crear documento Excel
  write.xlsx(Frec$NOMBRES, file = paste(Provincia,"xlsx",sep="."), sheetName = "Frecuencia")
  
  for (c in Canton) {
    print(c)
    CANTON <- Datos_de_c(PROVINCIA,c)
    # 1.3.1 Obtener las series de tiempo para el análisis de pronósticos del Cantón.
    Tipos_d <- names(table(PROVINCIA$Delito))
    Series <- A15_20(CANTON,Tipos_d)
    
    # 1.3.2 Análisis y Pronostico de Series
    ## Asignacion de fiscalias y delitos segun catalogo
    Fiscalias <- Asig_Fisc(Asignacion)
    Ser <- data.frame(Series[1])
    Ser[,2:length(Ser)] <- as.numeric(unlist(Ser[,2:length(Ser)])) ## cambio a terminos numericos las series 
    names(Ser)[1] <- "Tipo de Delito"
    Resultados <- Analisis(Ser,Asignacion)
    carga <- c() ## CARGA ASIGNADA A LA FISCALÍA.
    rm(F1,F2,F3,F4,F5,F6,F7,F8,F9)
    rm(r1,r2,r3,r4,r5,r6,r7,r8,r9)
    #Fiscalia 1
    
    if (dim(Resultados$DELFISC1)[1]==0) {
      r1 <- c("Fiscalia 1", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC1)[1]>0 & dim(Resultados$DELFISC1)[1]< 3) {
      F1 <- Cluster3(Resultados$DELFISC1,carga)
      r1 <- c("Fiscalia 1" , trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC1)[1] >= 3 & dim(Resultados$DELFISC1)[1]< 8) {
      F1 <- Cluster3(Resultados$DELFISC1,carga)
      r1 <- c("Fiscalia 1" , trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC1)[1] >= 8) {
      F1 <- Cluster(Resultados$DELFISC1,carga)
      r1 <- c("Fiscalia 1" , trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 2
    
    if (dim(Resultados$DELFISC2)[1]==0) {
      r2 <- c("Fiscalia 2", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC2)[1]>0 & dim(Resultados$DELFISC2)[1]< 3) {
      F2 <- Cluster3(Resultados$DELFISC2,carga)
      r2 <- c("Fiscalia 2" , trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC2)[1] >= 3 & dim(Resultados$DELFISC2)[1]< 8) {
      F2 <- Cluster3(Resultados$DELFISC2,carga)
      r2 <- c("Fiscalia 2" , trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC2)[1] >= 8) {
      F2 <- Cluster3(Resultados$DELFISC2,carga)
      r2 <- c("Fiscalia 2" , trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 3
    
    if (dim(Resultados$DELFISC3)[1]==0) {
      r3 <- c("Fiscalia 3", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC3)[1]>0 & dim(Resultados$DELFISC3)[1]< 3) {
      F3 <- Cluster3(Resultados$DELFISC3,carga)
      r3 <- c("Fiscalia 3" , trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC3)[1] >= 3 & dim(Resultados$DELFISC3)[1]< 8) {
      F3 <- Cluster3(Resultados$DELFISC3,carga)
      r3 <- c("Fiscalia 3" , trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC3)[1] >= 8) {
      F3 <- Cluster3(Resultados$DELFISC3,carga)
      r3 <- c("Fiscalia 3" , trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 4
    
    if (dim(Resultados$DELFISC4)[1]==0) {
      r4 <- c("Fiscalia 4", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC4)[1]>0 & dim(Resultados$DELFISC4)[1]< 3) {
      F4 <- Cluster3(Resultados$DELFISC4,carga)
      r4 <- c("Fiscalia 4" , trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC4)[1] >= 3 & dim(Resultados$DELFISC4)[1]< 8) {
      F4 <- Cluster3(Resultados$DELFISC4,carga)
      r4 <- c("Fiscalia 4" , trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC4)[1] >= 8) {
      F4 <- Cluster3(Resultados$DELFISC4,carga)
      r4 <- c("Fiscalia 4" , trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 5
    
    if (dim(Resultados$DELFISC5)[1]==0) {
      r5 <- c("Fiscalia 5", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC5)[1]>0 & dim(Resultados$DELFISC5)[1]< 3) {
      F5 <- Cluster3(Resultados$DELFISC5,carga)
      r5 <- c("Fiscalia 5" , trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC5)[1] >= 3 & dim(Resultados$DELFISC5)[1]< 8) {
      F5 <- Cluster3(Resultados$DELFISC5,carga)
      r5 <- c("Fiscalia 5" , trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC5)[1] >= 8) {
      F5 <- Cluster3(Resultados$DELFISC5,carga)
      r5 <- c("Fiscalia 5" , trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 6
    
    if (dim(Resultados$DELFISC6)[1]==0) {
      r6 <- c("Fiscalia 6", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC6)[1]>0 & dim(Resultados$DELFISC6)[1]< 3) {
      F6 <- Cluster3(Resultados$DELFISC6,carga)
      r6 <- c("Fiscalia 6" , trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC6)[1] >= 3 & dim(Resultados$DELFISC6)[1]< 8) {
      F6 <- Cluster3(Resultados$DELFISC6,carga)
      r6 <- c("Fiscalia 6" , trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC6)[1] >= 8) {
      F6 <- Cluster3(Resultados$DELFISC6,carga)
      r6 <- c("Fiscalia 6" , trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 7 
    
    if (dim(Resultados$DELFISC7)[1]==0) {
      r7 <- c("Fiscalia 7", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC7)[1]>0 & dim(Resultados$DELFISC7)[1]< 3) {
      F7 <- Cluster3(Resultados$DELFISC7,carga)
      r7 <- c("Fiscalia 7" , trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC7)[1] >= 3 & dim(Resultados$DELFISC7)[1]< 8) {
      F7 <- Cluster3(Resultados$DELFISC7,carga)
      r7 <- c("Fiscalia 7" , trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC7)[1] >= 8) {
      F7 <- Cluster3(Resultados$DELFISC7,carga)
      r7 <- c("Fiscalia 7" , trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 8
    
    if (dim(Resultados$DELFISC8)[1]==0) {
      r8 <- c("Fiscalia 8", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC8)[1]>0 & dim(Resultados$DELFISC8)[1]< 3) {
      F8 <- Cluster3(Resultados$DELFISC8,carga)
      r8 <- c("Fiscalia 8" , trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC8)[1] >= 3 & dim(Resultados$DELFISC8)[1]< 8) {
      F8 <- Cluster3(Resultados$DELFISC8,carga)
      r8 <- c("Fiscalia 8" , trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC8)[1] >= 8) {
      F8 <- Cluster3(Resultados$DELFISC8,carga)
      r8 <- c("Fiscalia 8" , trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo3))))
    }
    
    #Fiscalia 9
    
    if (dim(Resultados$DELFISC9)[1]==0) {
      r9 <- c("Fiscalia 9", 0, 0 ,0) 
    } 
    if (dim(Resultados$DELFISC9)[1]>0 & dim(Resultados$DELFISC9)[1]< 3) {
      F9 <- Cluster3(Resultados$DELFISC9,carga)
      r9 <- c("Fiscalia 9" , trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC9)[1] >= 3 & dim(Resultados$DELFISC9)[1]< 8) {
      F9 <- Cluster3(Resultados$DELFISC9,carga)
      r9 <- c("Fiscalia 9" , trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo3))))
    } 
    if (dim(Resultados$DELFISC9)[1] >= 8) {
      F9 <- Cluster3(Resultados$DELFISC9,carga)
      r9 <- c("Fiscalia 9" , trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo3))))
    }
    
    
    Demanda <- rbind.data.frame(r1,r2,r3,r4,r5,r6,r7,r8,r9)
    Demanda[,2:length(Demanda)] <- as.numeric(unlist(Demanda[,2:length(Demanda)]))
    colnames(Demanda) <- c("Fiscalias", "Grupo1" , "Grupo2" , "Grupo3")
    
    write.xlsx(Demanda, file = paste(Provincia,"xlsx",sep="."), sheetName = c, append = T )
  }
  
  return(DEm =Demanda)
}


