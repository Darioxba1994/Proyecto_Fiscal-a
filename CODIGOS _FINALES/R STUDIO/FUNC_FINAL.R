### Funciones Necesarias para correr todo
source("Lectura_Datos.R") ## 2 minutos en leer bases de datos
setwd("~/DOC_TESIS_FINAL/CÓDIGOS R") 

#f1

Datos_de_p  <- function(DXSiaf,Provincia){
  PROVINCIA <- DXSiaf[DXSiaf$Provincia == Provincia , ]
  return(PROVINCIA)
}

#f2

Grafico_P <- function(Provincia,PROVINCIA){
  library(ggplot2)
  library(ggspatial)
  library(sf)
  library(stringr)
  ## Transformar los datos a cordenadas
  cantones <- st_read("ecuador.json")
  PROVINCIA1 <- cantones[cantones$DPA_DESPRO == chartr("Ñ", "Ð", toupper(Provincia)),]
  cantones_centroides <- sf::st_centroid(PROVINCIA1)
  Nom_Can <- cantones_centroides$DPA_DESCAN
  cantones_centroides_xy <- as.data.frame(sf::st_coordinates(cantones_centroides))
  cantones_centroides_xy$canton <- Nom_Can ## asigno los valores en cada uno 
  Nom_Can <- chartr("ÁÉÍÓÚÑÐ", "AEIOUNN", toupper(Nom_Can))
  ## grafico el mapa.
  DEL_PROV <- as.data.frame(table(PROVINCIA$`CANTÓN DEL INCIDENTE`)) ## Datos de Frecuencia
  names(DEL_PROV) <- c("Nom_Can", "Frecuencia")
  ## Correcciones
  Nom_Can <- chartr("ÁÉÍÓÚÑÐ", "AEIOUNN", toupper(Nom_Can))
  Nom_Can <- gsub( "GNRAL. ", "", Nom_Can)
  Nom_Can <- gsub( "CRNEL. ", "", Nom_Can)
  Nom_Can <- gsub( "RIOVERDE", "RIO VERDE", Nom_Can)
  Nom_Can <- gsub( "PUEBLOVIEJO", "PUEBLO VIEJO", Nom_Can)
  Nom_Can <- gsub( "SALCEDO", "SAN MIGUEL DE SALCEDO", Nom_Can)
  Nom_Can <- gsub( "CHIMBO", "SAN JOSE DE CHIMBO", Nom_Can)
  Nom_Can <- gsub( "ECHEANDIA", "ECHANDíA", Nom_Can)
  
  Nom_Can <- gsub( "PABLO SEXTO", "PABLO VI", Nom_Can)
  Nom_Can <- gsub( "PALORA", "PALORA ( METZERA )", Nom_Can)
  Nom_Can <- gsub( "SANTIAGO", "SANTIAGO (MENDEZ)", Nom_Can)
  
  Nom_Can <- gsub( "PASTAZA", "PASTAZA (PUYO)", Nom_Can)
  
  Nom_Can <- gsub( "ORELLANA", "PUERTO FRANCISCO DE ORELLANA", Nom_Can)
  
  Nom_Can <- gsub( "CASCALES", "CASCALES ( EL DORADO )", Nom_Can)
  Nom_Can <- gsub( "LAGO AGRIO", "LAGO AGRIO ( NUEVA LOJA )", Nom_Can)
  Nom_Can <- gsub( "PUTUMAYO", "PUERTO EL CARMEN DE PUTUMAYO", Nom_Can)
  
  DEL_PROV <- merge(as.data.frame(Nom_Can), DEL_PROV, by = c("Nom_Can"), all.x = TRUE)
  DEL_PROV[is.na(DEL_PROV)] <- 0
  
  ## asignar los datos al grafico
  y <- c()
  for (i  in 1:length(Nom_Can)) {
    for (j in 1:dim(DEL_PROV)[1] ) {
      if (DEL_PROV$Nom_Can[j] == Nom_Can[i]) {
        y <- c(y,DEL_PROV$Frecuencia[j])
      } 
    }
  }
  DEL_PROV <- arrange(DEL_PROV, desc(Frecuencia))
  return(list(CANTIDAD=y,NOMBRES= DEL_PROV, COORDE = cantones_centroides_xy)) ## RETORNO VALORES DE FRECUENCIA Y LA BASE DE DATOS DE LA PROVINCIA SELECCIONADA
}

#f3

Datos_de_c  <- function(PROVINCIA, Canton){
  CANTON <- PROVINCIA[PROVINCIA$`CANTÓN DEL INCIDENTE` == Canton , ]
  return(CANTON)
}

#f4

A15_21 <-  function(CANTON,Tipos_d){
  library(stringr)
  CANTON <- CANTON[ ,c("Delito", "Fecha_Incidente")]
  CANTON$AÑO <- format(as.Date(CANTON$Fecha_Incidente), '%Y')
  CANTON$MES <- format(as.Date(CANTON$Fecha_Incidente), '%m')
  
  
  QxM <- data.frame() ## LLENAREMOS DE DELITOS POR MES 
  QxA <- data.frame() ## LLENAREMOS DE DELITOS POR AÑO 
  
  AÑOS <- c("2015","2016","2017","2018","2019","2020", "2021" ) ## AÑOS EN ESTUDIO(  i)
  MESES <- c("01", "02","03", "04","05", "06","07", "08","09", "10","11", "12") ## Nro DE MES (j) 
  
  for (d in 1:length(Tipos_d)){
    if (dim(CANTON[CANTON$Delito == Tipos_d[d],])[1] > 0) {
      delitos <- CANTON[CANTON$Delito == Tipos_d[d],]
      a1 <- c()
      a2 <- c()
      for (i in 1:length(AÑOS)) {
        A <- delitos[delitos$AÑO == AÑOS[i] ,]
        a1 <- c(a1,dim(A)[1])
        for (j in 1:length(MESES)) {
          M <- A[A$MES == MESES[j],]
          a2 <- c(a2,length(M$MES))
        }
      }
      QxA <-  rbind(QxA, c(Tipos_d[d],a1))
      QxM <-  rbind(QxM, c(Tipos_d[d],a2))
    } else{
      QxA <-  rbind(QxA, c(Tipos_d[d],rep(0,7)))
      QxM <-  rbind(QxM, c(Tipos_d[d],rep(0,7*12)))
    }
    
  }
  QxMname <- c()
  for (i in 1:6) {
    for (j in 1:12){
      QxMname <- c(QxMname, str_c(AÑOS[i], MESES[j]))
    }
  }
  colnames(QxA) <- c("Tipo de Delito",AÑOS)
  
  
  colnames(QxM) <- c("Tipo de Delito",QxMname)
  return(list(DelxMes = QxM, DelxAño = QxA))
}

#f5

Asig_Fisc <- function(Asignacion){
  
  Fis <- as.data.frame(table(Asignacion$Fiscalía))
  Fis <- arrange(Fis, desc(Freq))
  Fis <- cbind(c(1:9), Fis)
  names(Fis) <- c("Numero de Fiscalía","Nombre de Fiscalía", "Cantidad de Delitos")
  
  Fis1 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE PERSONAS Y GARANTIAS" ,]$Delito
  Fis2 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE DELINCUENCIA ORGANIZADA, TRANSNACIONAL E INTERNACIONAL" ,]$Delito
  Fis3 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE ADMINISTRACION PUBLICA" ,]$Delito
  Fis4 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE SOLUCIONES RAPIDAS" ,]$Delito
  Fis5 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE VIOLENCIA DE GENERO" ,]$Delito
  Fis6 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE PATRIMONIO CIUDADANO" ,]$Delito
  Fis7 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE FE  PUBLICA" ,]$Delito
  Fis8 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE ACCIDENTES DE TRANSITO" ,]$Delito
  Fis9 <- Asignacion[Asignacion$Fiscalía == "UNIDAD ANTILAVADO DE ACTIVOS" ,]$Delito
  return(list(FISC1 = Fis1, FISC2 = Fis2, FISC3 = Fis3,FISC4 = Fis4, FISC5 = Fis5,FISC6 = Fis6,FISC7 = Fis7,FISC8 = Fis8,FISC9 = Fis9))
}

#f6

Analisis <- function(Ser, Asignacion){
  
  library(stats) ## Característiscas estadísticas
  library(tsibble)
  library(lubridate)
  
  ## 1  Creo las asignaciónes de las Fiscalías.
  Fis1 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE PERSONAS Y GARANTIAS" ,]$Delito
  Fis2 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE DELINCUENCIA ORGANIZADA, TRANSNACIONAL E INTERNACIONAL" ,]$Delito
  Fis3 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE ADMINISTRACION PUBLICA" ,]$Delito
  Fis4 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE SOLUCIONES RAPIDAS" ,]$Delito
  Fis5 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE VIOLENCIA DE GENERO" ,]$Delito
  Fis6 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE PATRIMONIO CIUDADANO" ,]$Delito
  Fis7 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE FE  PUBLICA" ,]$Delito
  Fis8 <- Asignacion[Asignacion$Fiscalía == "FISCALIA DE ACCIDENTES DE TRANSITO" ,]$Delito
  Fis9 <- Asignacion[Asignacion$Fiscalía == "UNIDAD ANTILAVADO DE ACTIVOS" ,]$Delito
  
  intersect(Fis4,Fis6)
  
  ## 2 Datos para la fiscalía 1
  
  DelF1 <- as.data.frame(Fis1)
  colnames(DelF1) <- c("Tipo de Delito")
  n1 <- merge(Ser, DelF1, by = c("Tipo de Delito"))
  n1 <- n1[n1$`Tipo de Delito` != "FEMICIDIO" & n1$`Tipo de Delito` != "VIOLACION INCESTUOSA",]
  # Transpone todas las columnas menos la primer
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2021,12), by = "month")))
  colnames(Fechas) <- c("Fechas")
  df_transpose <- cbind.data.frame(Fechas, df_transpose)
  # Dissimilarity matrix
  if (length(df_transpose)>= 2.1) {
    DxA <- as.data.frame(colSums(df_transpose[1:84,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c()
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n1[,73:84])))} else{
      
      n <- data.frame()
    }    
  
  ## 2 Datos para la fiscalía 2
  
  DelF1 <- as.data.frame(Fis2)
  colnames(DelF1) <- c("Tipo de Delito")
  n1 <- merge(Ser , DelF1, by = c("Tipo de Delito"))
  # Transpone todas las columnas menos la primer
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  df_transpose <- cbind(Fechas, df_transpose)
  # Dissimilarity matrix
  if (length(df_transpose)>= 2.1) {
    DxA <- as.data.frame(colSums(df_transpose[1:84,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n2 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n2[,73:84])))} else{
      
      n2 <- data.frame()
    }    
  
  ## 3 Datos para la fiscalía 3
  
  DelF1 <- as.data.frame(Fis3)
  colnames(DelF1) <- c("Tipo de Delito")
  n1 <- merge(Ser , DelF1, by = c("Tipo de Delito"))
  n1 <- n1[n1$`Tipo de Delito` != "ROBO" & n1$`Tipo de Delito` != "VIOLACION DE PROPIEDAD PRIVADA", ]
  # Transpone todas las columnas menos la primer
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  df_transpose <- cbind(Fechas, df_transpose)
  # Dissimilarity matrix
  if (length(df_transpose)>= 2.1) {
    DxA <- as.data.frame(colSums(df_transpose[1:84,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n3 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n3[,73:84])))} else{
      
      n3 <- data.frame()
    }    
  
  ## 4 Datos para la fiscalía 4
  
  DelF1 <- as.data.frame(Fis4)
  colnames(DelF1) <- c("Tipo de Delito")
  n1 <- merge(Ser, DelF1, by = c("Tipo de Delito"))
  n1 <- n1[n1$`Tipo de Delito` != "ABANDONO DE PERSONA (EN CASO DE PERSONAS EN SITUACIONES VULNERABLES)" & n1$`Tipo de Delito` != "EXTRALIMITACION EN LA EJECUCION DE UN ACTO DE SERVICIO" & n1$`Tipo de Delito` != "LESIONES",]
  n1 <- n1[n1$`Tipo de Delito` != "DAÑO A BIEN AJENO",]
  # Transpone todas las columnas menos la primer
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  df_transpose <- cbind(Fechas, df_transpose)
  # Dissimilarity matrix
  if (length(df_transpose)>= 2.1) {
    DxA <- as.data.frame(colSums(df_transpose[1:84,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n4 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n4[,73:84])))} else{
      
      n4 <- data.frame()
    }    
  
  ## 5 Datos para la fiscalía 5
  
  DelF1 <- as.data.frame(Fis5)
  colnames(DelF1) <- c("Tipo de Delito")
  n1 <- merge(Ser , DelF1, by = c("Tipo de Delito"))
  # Transpone todas las columnas menos la primer
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  df_transpose <- cbind(Fechas, df_transpose)
  # Dissimilarity matrix
  if (length(df_transpose)>= 2.1) {
    DxA <- as.data.frame(colSums(df_transpose[1:84,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n5 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n5[,73:84])))} else{
      
      n5 <- data.frame()
    }    
  
  ## 6 Datos para la fiscalía 6  
  
  DelF1 <- as.data.frame(Fis6)
  colnames(DelF1) <- c("Tipo de Delito")
  n1 <- merge(Ser , DelF1, by = c("Tipo de Delito"))  
  n1 <- n1[n1$`Tipo de Delito` != "ROBO",]
  n1 <- n1[n1$`Tipo de Delito` != "ESTAFA",]
  # transponemos
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]  
  df_transpose <- cbind(Fechas, df_transpose)
  # Dissimilarity matrix
  if (length(df_transpose)>= 2.1) {
    DxA <- as.data.frame(colSums(df_transpose[1:84,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n6 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n6[,73:84])))} else{
      
      n6 <- data.frame()
    }    
  
  ## 7 Datos para la fiscalía 7  
  
  DelF1 <- as.data.frame(Fis7)
  colnames(DelF1) <- c("Tipo de Delito")
  n1 <- merge(Ser , DelF1, by = c("Tipo de Delito"))  
  # Añadimos los nombres de las columnas
  # transponemos
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]  
  df_transpose <- cbind(Fechas, df_transpose)
  # Dissimilarity matrix
  if (length(df_transpose)>= 2.1) {
    DxA <- as.data.frame(colSums(df_transpose[1:84,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n7 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n7[,73:84]))) } else{
      
      n7 <- data.frame()
    }    
  
  ## 8 Datos para la fiscalía 8  
  
  DelF1 <- as.data.frame(Fis8)
  colnames(DelF1) <- c("Tipo de Delito")
  n1 <- merge(Ser , DelF1, by = c("Tipo de Delito"))
  # transponemos
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]  
  df_transpose <- cbind(Fechas, df_transpose)
  # Dissimilarity matrix
  if (length(df_transpose)>= 2.1) {
    DxA <- as.data.frame(colSums(df_transpose[1:84,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n8 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n8[,73:84])))} else{
      
      n8 <- data.frame()
    }       
  
  
  ## 9 Datos para la fiscalía 9  
  
  DelF1 <- as.data.frame(Fis9)
  colnames(DelF1) <- c("Tipo de Delito")
  n1 <- merge(Ser , DelF1, by = c("Tipo de Delito"))
  # transponemos
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]  
  df_transpose <- cbind(Fechas, df_transpose)
  # Dissimilarity matrix
  if (length(df_transpose)>= 2.1) {
    DxA <- as.data.frame(colSums(df_transpose[1:84,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n9 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n9[,73:84])))} else{
      
      n9 <- data.frame()
    }
  
  
  return(list(DELFISC1 = n, DELFISC2 = n2, DELFISC3 = n3, DELFISC4 = n4, DELFISC5 = n5, DELFISC6 = n6, DELFISC7 = n7, DELFISC8 = n8, DELFISC9 = n9, Datos_Real = Datos_Reales))
}

#f7

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
  total <- c("grupo", "lt", "bt", "s1","s2","s3", "sse","alpha", "beta", "gamma", "MAPE")
  nam <- c("Canton")
  for (c in Canton) {
    print(c)
    CANTON <- Datos_de_c(PROVINCIA,c)
    # 1.3.1 Obtener las series de tiempo para el análisis de pronósticos del Cantón.
    Tipos_d <- names(table(PROVINCIA$Delito))
    Series <- A15_21(CANTON,Tipos_d)
    
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
    } else{
      F1 <- try(cluster(Resultados$DELFISC1,carga), silent=TRUE)
      if ('try-error' %in% class(F1)) {
        F1 <-  try(Cluster2(Resultados$DELFISC1,carga), silent=TRUE)
        if ('try-error' %in% class(F1)) {
          F1 <- Cluster3(Resultados$DELFISC1,carga)
          r1 <- c("Fiscalia 1" , trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo3))))
        }else{
          r1 <- c("Fiscalia 1" , trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo3))))
        }
      } else{ 
        r1 <- c("Fiscalia 1" , trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F1$Pronosticos$Prediccion_Grupo3))))
      }
    }
    
    tot1 <- c(F1$Paramet)
    
    #Fiscalia 2
    
    if (dim(Resultados$DELFISC2)[1]==0) {
      r2 <- c("Fiscalia 2", 0, 0 ,0) 
    } else{
      F2 <- try(cluster(Resultados$DELFISC2,carga), silent=TRUE)
      if ('try-error' %in% class(F2)) {
        F2 <- try(Cluster2(Resultados$DELFISC2,carga), silent=TRUE)
        if ('try-error' %in% class(F2)) {
          F2 <- Cluster3(Resultados$DELFISC2,carga)
          r2 <- c("Fiscalia 2" , trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo3))))
        }else{
          r2 <- c("Fiscalia 2" , trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo3))))
        }
      } else{ 
        r2 <- c("Fiscalia 2" , trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F2$Pronosticos$Prediccion_Grupo3))))
      }
    }
    tot1 <- rbind(tot1,F2$Paramet)
    
    #Fiscalia 3
    
    if (dim(Resultados$DELFISC3)[1]==0) {
      r3 <- c("Fiscalia 3", 0, 0 ,0) 
    } else{
      F3 <- try(Cluster(Resultados$DELFISC3,carga), silent=TRUE)
      if ('try-error' %in% class(F3)) {
        F3 <- try(Cluster2(Resultados$DELFISC3,carga), silent=TRUE)
        if ('try-error' %in% class(F3)) {
          F3 <- Cluster3(Resultados$DELFISC3,carga)
          r3 <- c("Fiscalia 3" , trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo3))))
        }else{
          r3 <- c("Fiscalia 3" , trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo3))))
        }
      } else{ 
        r3 <- c("Fiscalia 3" , trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F3$Pronosticos$Prediccion_Grupo3))))
      }
    }
    tot1 <- rbind(tot1,F3$Paramet)
    
    #Fiscalia 4
    
    if (dim(Resultados$DELFISC4)[1]==0) {
      r4 <- c("Fiscalia 4", 0, 0 ,0) 
    } else{
      F4 <- try(Cluster(Resultados$DELFISC4,carga), silent=TRUE)
      if ('try-error' %in% class(F4)) {
        F4 <- try(Cluster2(Resultados$DELFISC4,carga), silent=TRUE)
        if ('try-error' %in% class(F4)) {
          F4 <- Cluster3(Resultados$DELFISC4,carga)
          r4 <- c("Fiscalia 4" , trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo3))))
        }else{
          r4 <- c("Fiscalia 4" , trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo3))))
        }
      } else{ 
        r4 <- c("Fiscalia 4" , trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F4$Pronosticos$Prediccion_Grupo3))))
      }
    }
    
    tot1 <- rbind(tot1,F4$Paramet)
    #Fiscalia 5
    
    if (dim(Resultados$DELFISC5)[1]==0) {
      r5 <- c("Fiscalia 5", 0, 0 ,0) 
    } else{
      F5 <- try(Cluster(Resultados$DELFISC5,carga), silent=TRUE)
      if ('try-error' %in% class(F5)) {
        F5 <- try(Cluster2(Resultados$DELFISC5,carga), silent=TRUE)
        if ('try-error' %in% class(F5)) {
          F5 <- Cluster3(Resultados$DELFISC5,carga)
          r5 <- c("Fiscalia 5" , trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo3))))
        }else{
          r5 <- c("Fiscalia 5" , trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo3))))
        }
      } else{ 
        r5 <- c("Fiscalia 5" , trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F5$Pronosticos$Prediccion_Grupo3))))
      }
    }
    
    tot1 <- rbind(tot1,F5$Paramet)
    #Fiscalia 6
    
    if (dim(Resultados$DELFISC6)[1] ==0) {
      r6 <- c("Fiscalia 6", 0, 0 ,0) 
    } else{
      F6 <- try(Cluster(Resultados$DELFISC6,carga), silent=TRUE)
      if ('try-error' %in% class(F6)) {
        F6 <- try(Cluster2(Resultados$DELFISC6,carga), silent=TRUE)
        if ('try-error' %in% class(F6)) {
          F6 <- Cluster3(Resultados$DELFISC6,carga)
          r6 <- c("Fiscalia 6" , trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo3))))
        }else{
          r6 <- c("Fiscalia 6" , trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo3))))
        }
      } else{ 
        r6 <- c("Fiscalia 6" , trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F6$Pronosticos$Prediccion_Grupo3))))
      }
      
    }
    tot1 <- rbind(tot1,F6$Paramet)
    #Fiscalia 7 
    
    if (dim(Resultados$DELFISC7)[1]==0) {
      r7 <- c("Fiscalia 7", 0, 0 ,0) 
    } else{
      F7 <- try(Cluster(Resultados$DELFISC7,carga), silent=TRUE)
      if ('try-error' %in% class(F7)) {
        F7 <- try(Cluster2(Resultados$DELFISC7,carga), silent=TRUE)
        if ('try-error' %in% class(F7)) {
          F7 <- Cluster3(Resultados$DELFISC7,carga)
          r7 <- c("Fiscalia 7" , trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo3))))
        }else{
          r7 <- c("Fiscalia 7" , trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo3))))
        }
      } else{ 
        r7 <- c("Fiscalia 7" , trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F7$Pronosticos$Prediccion_Grupo3))))
      }
    }
    
    tot1 <- rbind(tot1,F7$Paramet)
    #Fiscalia 8
    
    if (dim(Resultados$DELFISC8)[1]==0) {
      r8 <- c("Fiscalia 8", 0, 0 ,0) 
    } else{
      F8 <- try(Cluster(Resultados$DELFISC8,carga), silent=TRUE)
      if ('try-error' %in% class(F8)) {
        F8 <- try(Cluster2(Resultados$DELFISC8,carga), silent=TRUE)
        if ('try-error' %in% class(F8)) {
          F8 <- Cluster3(Resultados$DELFISC8,carga)
          r8 <- c("Fiscalia 8" , trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo3))))
        }else{
          r8 <- c("Fiscalia 8" , trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo3))))
        }
      } else{ 
        r8 <- c("Fiscalia 8" , trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F8$Pronosticos$Prediccion_Grupo3))))
      }
    }
    tot1 <- rbind(tot1,F8$Paramet)
    #Fiscalia 9
    
    if (dim(Resultados$DELFISC9)[1]==0) {
      r9 <- c("Fiscalia 9", 0, 0 ,0) 
    } else{
      
      F9 <- try(Cluster(Resultados$DELFISC9,carga), silent=TRUE)
      if ('try-error' %in% class(F9)) {
        F9 <- try(Cluster2(Resultados$DELFISC9,carga), silent=TRUE)
        if ('try-error' %in% class(F9)) {
          F9 <- Cluster3(Resultados$DELFISC9,carga)
          r9 <- c("Fiscalia 9" , trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo3))))
        }else{
          r9 <- c("Fiscalia 9" , trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo3))))
        }
      } else{ 
        r9 <- c("Fiscalia 9" , trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo1))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo2))), trunc(abs(sum(F9$Pronosticos$Prediccion_Grupo3))))
      }
      
    }
    
    Demanda <- rbind.data.frame(r1,r2,r3,r4,r5,r6,r7,r8,r9)
    Demanda[,2:length(Demanda)] <- as.numeric(unlist(Demanda[,2:length(Demanda)]))
    colnames(Demanda) <- c("Fiscalias", "Grupo1" , "Grupo2" , "Grupo3")
    
    write.xlsx(Demanda, file = paste(Provincia,"xlsx",sep="."), sheetName = c, append = T )
    nam <- c(nam,rep(c, dim(tot1)[1]))
    total <- rbind(total, tot1 )
  }
  total <- cbind(nam,total)
 return(total) 
}

Cluster <- function(n1,carga){
  library(factoextra)
  library(tidyverse)
  library(TSclust)
  library(cluster)
  ## cluster 01
  # Hierarchical clustering
  # Test del mejor método 
  # methods to assess
  m <- c( "average", "single", "complete", "ward")
  names(m) <- c( "average_eucl", "single_eucl", "complete_ucl", "ward_eucl")
  d <- c("average", "single", "complete", "ward")
  names(d) <- c("average_mah", "single_mah", "complete_mah", "ward_mah")
  agnes(n1[,2:84], method = "ward", metric = "manhattan")$ac
  
  # Hallar distancia 
  d <- dist(n1[,2:84], method = "manhattan")
  hc_euclidea_ward <- hclust(d = d, method = "ward.D" )
  
  # Plot the obtained dendrogram
  fviz_dend(x = hc_euclidea_ward , k = 3, cex = 0.6) +
    geom_hline(yintercept = 40, linetype = "dashed") +
    labs(title = "Herarchical clustering",
         subtitle = "Distancia euclídea, Lincage complete, K=3")
  
  ## Realiza los cluster 
  
  IP.hclus <- cutree(hc_euclidea_ward, k = 3)
  clust <- as.data.frame(IP.hclus)
  clust <- cbind.data.frame(n1[,1],clust)
  colnames(clust) <- c("Tipo de Delito", "Grupo")
  
  ### Division de los Grupos despues del cluster.
  clust1 <- as.data.frame(clust[clust$Grupo == 1 , ]$`Tipo de Delito`)
  colnames(clust1) <- c("Tipo de Delito")
  g1 <- merge(clust1,n1[,1:85], by = c("Tipo de Delito"))
  clust2 <- as.data.frame(clust[clust$Grupo == 2 , ]$`Tipo de Delito`)
  colnames(clust2) <- c("Tipo de Delito")
  g2 <- merge(clust2,n1[,1:85], by = c("Tipo de Delito"))
  clust3 <- as.data.frame(clust[clust$Grupo ==  3 , ]$`Tipo de Delito`)
  colnames(clust3) <- c("Tipo de Delito")
  g3 <- merge(clust3,n1[,1:85], by = c("Tipo de Delito"))
  # Transpone todas las columnas menos la primer
  Grupo1 <- data.frame(t(g1[-1]))
  Grupo2 <- data.frame(t(g2[-1]))
  Grupo3 <- data.frame(t(g3[-1]))
  # Añadimos los nombres de las columnas
  colnames(Grupo1 ) <- g1[, 1]
  colnames(Grupo2 ) <- g2[, 1]
  colnames(Grupo3 ) <- g3[, 1]
  
  
  
  library(feasts)
  library(forecast)
  library(reshape2)
  library(fable)
  library(tsibble)
  library(lubridate)
  st <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2021,12), by = "month")))
  colnames(Fechas) <- c("Fechas") ## Creo columna de formato fechas en tipo dataframe
  
  
  ## Analisis del grupo 1 
  
  Grupo1 <- cbind(Fechas,Grupo1[1:84,]) ## Uno los datos con las fechas
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo1)>= 3) {
    Grupo1$Seriestotal <- rowSums(Grupo1[,2:length(Grupo1)])
  } else{
    Grupo1$Seriestotal <- Grupo1[,2]
  }
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo1$Seriestotal[1:84],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  f <- c("1")
  Nombres_coef <- c("a","b", "s1", "s2","s3", "sse")
  for (c in Nombres_coef[1:5]) {
    a <- try(serie$coefficients[[c]], silent=TRUE)
    if ('try-error' %in% class(a)){
    f <- c(f,0)
    }else{
    f <- c(f, serie$coefficients[[c]])
    }
  }
  
  f <- c(f,serie$SSE)
  
  al <- try(serie$alpha[["alpha"]], silent=TRUE)
  if ('try-error' %in% class(al)){
    f <- c(f,0)
    #llenar coeficiente gamma
  }else{
    f <- c(f,serie$alpha[["alpha"]])
  }
  #llenar coeficiente alpha
  be <- try(serie$beta[["beta"]], silent=TRUE)
  if ('try-error' %in% class(be)){
    f <- c(f,0)
  }else{
    f <- c(f, serie$beta[["beta"]])
  }
  #llenar coeficiente beta
  ta <- try(serie$gamma[["gamma"]], silent=TRUE)
  if ('try-error' %in% class(ta)){
    f <- c(f,0)
  }else{
    f <- c(f, serie$gamma[["gamma"]])
  }
  
  st <- cbind(st,fore$upper[,1])
  
  ## Analisis del grupo 2 
  
  Grupo2 <- cbind(Fechas,Grupo2[1:84,]) ## Uno los datos con las fechas
  
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo2)>= 3) {
    Grupo2$Seriestotal <- rowSums(Grupo2[,2:length(Grupo2)])
  } else{
    Grupo2$Seriestotal <- Grupo2[,2]
  }
  
  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo2$Seriestotal[1:84],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo2$Seriestotal[1:84],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo2$Seriestotal[1:84],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  f1 <- c("2")
  for (c in Nombres_coef[1:5]) {
    a <- try(serie$coefficients[[c]], silent=TRUE)
    if ('try-error' %in% class(a)){
      f1 <- c(f1,0)
    }else{
      f1 <- c(f1, serie$coefficients[[c]])
    }
  }
  

  f1 <- c(f1,serie$SSE)
  al <- try(serie$alpha[["alpha"]], silent=TRUE)
  if ('try-error' %in% class(al)){
    f1 <- c(f1,0)
    #llenar coeficiente gamma
  }else{
    f1 <- c(f1,serie$alpha[["alpha"]])
  }
  #llenar coeficiente alpha
  be <- try(serie$beta[["beta"]], silent=TRUE)
  if ('try-error' %in% class(be)){
    f1 <- c(f1,0)
  }else{
    f1 <- c(f1, serie$beta[["beta"]])
  }
  #llenar coeficiente beta
  ta <- try(serie$gamma[["gamma"]], silent=TRUE)
  if ('try-error' %in% class(ta)){
    f1 <- c(f1,0)
  }else{
    f1 <- c(f1, serie$gamma[["gamma"]])
  }
  
  st <- cbind(st,fore$upper[,1])
  
  ## Análisis del grupo 3 de la fiscalía 
  
  ## Analisis del grupo 3
  
  Grupo3 <- cbind(Fechas,Grupo3[1:84,]) ## Uno los datos con las fechas
  
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo3)>= 3) {
    Grupo3$Seriestotal <- rowSums(Grupo3[,2:length(Grupo3)])
  } else{
    Grupo3$Seriestotal <- Grupo3[,2]
  }
  
  ## Descomposicion de la serie.
  comp <- decompose(ts(Grupo3$Seriestotal[1:84],frequency = 12)) ## Componentes de la serie 
  
  
  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo3$Seriestotal[1:84],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo3$Seriestotal[1:84],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo3$Seriestotal[1:84],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  
  f2 <- c("3")
  for (c in Nombres_coef[1:5]) {
    a <- try(serie$coefficients[[c]], silent=TRUE)
    if ('try-error' %in% class(a)){
      f2 <- c(f2,0)
    }else{
      f2 <- c(f2, serie$coefficients[[c]])
    }
  }

  f2 <- c(f2,serie$SSE)
  al <- try(serie$alpha[["alpha"]], silent=TRUE)
  if ('try-error' %in% class(al)){
    f2 <- c(f2,0)
    #llenar coeficiente gamma
  }else{
    f2 <- c(f2,serie$alpha[["alpha"]])
  }
  #llenar coeficiente alpha
  be <- try(serie$beta[["beta"]], silent=TRUE)
  if ('try-error' %in% class(be)){
    f2 <- c(f2,0)
  }else{
    f2 <- c(f2, serie$beta[["beta"]])
  }
  #llenar coeficiente beta
  ta <- try(serie$gamma[["gamma"]], silent=TRUE)
  if ('try-error' %in% class(ta)){
    f2 <- c(f2,0)
  }else{
    f2 <- c(f2, serie$gamma[["gamma"]])
  }
  
  st <- cbind(st,c(fore$mean))
  
  colnames(st) <- c("Mes", "Prediccion_Grupo1", "Prediccion_Grupo2", "Prediccion_Grupo3")
  ## MAPE
  mape <- c()
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo1,Grupo1$Seriestotal[73:84]))
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo2,Grupo2$Seriestotal[73:84]))
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo3,Grupo3$Seriestotal[73:84]))
  library(dplyr)
  mapes <- mape
  mape <- c("MAPE", mape)
  names(mape) <- c("Indicador", "Prediccion_Grupo1", "Prediccion_Grupo2", "Prediccion_Grupo3") 
  st$Total <- rowSums(st[,2:length(st)])
  PxF <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  RxF <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  # Transpone todas las columnas menos la primer
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  df_transpose$Total <- rowSums(df_transpose[,1:length(df_transpose)])
  RxF <- cbind(RxF,df_transpose$Total[73:84])
  PxF <- cbind(PxF,st$Total)
  
  carga <- c(carga, ceiling(sum(colSums(st[,2:(length(st)-1)]))))
  asd <- rbind(f,f1)
  asd <- rbind(asd,f2)
  asd <- cbind(asd, mapes)
  print(asd)
  return(list(Pronosticos = st , Carg = carga, MAPE = mape, Paramet = asd))
}

Cluster2 <- function(n1,carga){
  library(factoextra)
  library(tidyverse)
  library(TSclust)
  library(cluster)
  ## cluster 01
  # Hierarchical clustering
  # Test del mejor método 
  # methods to assess
  m <- c( "average", "single", "complete", "ward")
  names(m) <- c( "average_eucl", "single_eucl", "complete_ucl", "ward_eucl")
  d <- c("average", "single", "complete", "ward")
  names(d) <- c("average_mah", "single_mah", "complete_mah", "ward_mah")
  agnes(n1[,2:84], method = "ward", metric = "manhattan")$ac
  
  # Hallar distancia 
  d <- dist(n1[,2:84], method = "manhattan")
  hc_euclidea_ward <- hclust(d = d, method = "ward.D" )
  
  # Plot the obtained dendrogram
  fviz_dend(x = hc_euclidea_ward , k = 2, cex = 0.6) +
    geom_hline(yintercept = 40, linetype = "dashed") +
    labs(title = "Herarchical clustering",
         subtitle = "Distancia euclídea, Lincage complete, K=2")
  
  ## Realiza los cluster 
  
  IP.hclus <- cutree(hc_euclidea_ward, k = 2)
  clust <- as.data.frame(IP.hclus)
  clust <- cbind.data.frame(n1[,1],clust)
  colnames(clust) <- c("Tipo de Delito", "Grupo")
  
  ### Division de los Grupos despues del cluster.
  clust1 <- as.data.frame(clust[clust$Grupo == 1 , ]$`Tipo de Delito`)
  colnames(clust1) <- c("Tipo de Delito")
  g1 <- merge(clust1,n1[,1:85], by = c("Tipo de Delito"))
  clust2 <- as.data.frame(clust[clust$Grupo == 2 , ]$`Tipo de Delito`)
  colnames(clust2) <- c("Tipo de Delito")
  g2 <- merge(clust2,n1[,1:85], by = c("Tipo de Delito"))
  # Transpone todas las columnas menos la primer
  Grupo1 <- data.frame(t(g1[-1]))
  Grupo2 <- data.frame(t(g2[-1]))
  # Añadimos los nombres de las columnas
  colnames(Grupo1 ) <- g1[, 1]
  colnames(Grupo2 ) <- g2[, 1]
  
  library(feasts)
  library(forecast)
  library(reshape2)
  library(fable)
  library(tsibble)
  library(lubridate)
  st <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2021,12), by = "month")))
  colnames(Fechas) <- c("Fechas") ## Creo columna de formato fechas en tipo dataframe
  
  
  ## Analisis del grupo 1 
  
  Grupo1 <- cbind(Fechas,Grupo1[1:72,]) ## Uno los datos con las fechas
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo1)>= 3) {
    Grupo1$Seriestotal <- rowSums(Grupo1[,2:length(Grupo1)])
  } else{
    Grupo1$Seriestotal <- Grupo1[,2]
  }
  
  
  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo1$Seriestotal[1:84],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo1$Seriestotal[1:84],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo1$Seriestotal[1:84],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  f <- c(canton, "1")
  Nombres_coef <- c("a","b", "s1", "s2","s3", "sse")
  for (c in Nombres_coef[1:5]) {
    a <- try(serie$coefficients[[c]], silent=TRUE)
    if ('try-error' %in% class(a)){
      f <- c(f,0)
    }else{
      f <- c(f, serie$coefficients[[c]])
    }
  }

  f <- c(f,serie$SSE)
  al <- try(serie$alpha[["alpha"]], silent=TRUE)
  if ('try-error' %in% class(al)){
    f <- c(f,0)
    #llenar coeficiente gamma
  }else{
    f <- c(f,serie$alpha[["alpha"]])
  }
  #llenar coeficiente alpha
  be <- try(serie$beta[["beta"]], silent=TRUE)
  if ('try-error' %in% class(be)){
    f <- c(f,0)
  }else{
    f <- c(f, serie$beta[["beta"]])
  }
  #llenar coeficiente beta
  ta <- try(serie$gamma[["gamma"]], silent=TRUE)
  if ('try-error' %in% class(ta)){
    f <- c(f,0)
  }else{
    f <- c(f, serie$gamma[["gamma"]])
  }
  st <- cbind(st,c(fore$mean))
  
  ## Analisis del grupo 2 
  
  Grupo2 <- cbind(Fechas,Grupo2[1:84,]) ## Uno los datos con las fechas
  
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo2)>= 3) {
    Grupo2$Seriestotal <- rowSums(Grupo2[,2:length(Grupo2)])
  } else{
    Grupo2$Seriestotal <- Grupo2[,2]
  }
  
  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo2$Seriestotal[1:84],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo2$Seriestotal[1:84],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo2$Seriestotal[1:84],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  f1 <- c("2")
  Nombres_coef <- c("a","b", "s1", "s2","s3", "sse")
  for (c in Nombres_coef[1:5]) {
    a <- try(serie$coefficients[[c]], silent=TRUE)
    if ('try-error' %in% class(a)){
      f1 <- c(f1,0)
    }else{
      f1 <- c(f1, serie$coefficients[[c]])
    }
  }
  f1 <- c(f1,serie$SSE)
  
  al <- try(serie$alpha[["alpha"]], silent=TRUE)
  if ('try-error' %in% class(al)){
    f1 <- c(f1,0)
    #llenar coeficiente gamma
  }else{
    f1 <- c(f1,serie$alpha[["alpha"]])
  }
  #llenar coeficiente alpha
  be <- try(serie$beta[["beta"]], silent=TRUE)
  if ('try-error' %in% class(be)){
    f1 <- c(f1,0)
  }else{
    f1 <- c(f1, serie$beta[["beta"]])
  }
  #llenar coeficiente beta
  ta <- try(serie$gamma[["gamma"]], silent=TRUE)
  if ('try-error' %in% class(ta)){
    f1 <- c(f1,0)
  }else{
    f1 <- c(f1, serie$gamma[["gamma"]])
  }
  st <- cbind(st,fore$upper[,2])
  
  colnames(st) <- c("Mes", "Prediccion_Grupo1", "Prediccion_Grupo2")
  ## MAPE
  mape <- c()
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo1,Grupo1$Seriestotal[73:84]))
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo2,Grupo2$Seriestotal[73:84]))
  library(dplyr)
  mapes <- mape
  mape <- c("MAPE", mape)
  names(mape) <- c("Indicador", "Prediccion_Grupo1", "Prediccion_Grupo2") 
  st$Total <- rowSums(st[,2:length(st)])
  PxF <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  RxF <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  # Transpone todas las columnas menos la primer
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  df_transpose$Total <- rowSums(df_transpose[,1:length(df_transpose)])
  RxF <- cbind(RxF,df_transpose$Total[61:84])
  PxF <- cbind(PxF,st$Total)
  
  carga <- c(carga, ceiling(sum(colSums(st[,2:(length(st)-1)]))))
  asd <- rbind(f,f1)
  asd <- cbind(asd, mapes)
  return(list(Pronosticos = st , Carg = carga, MAPE = mape, Paramet = asd))
}

Cluster3 <- function(n1,carga){
  library(factoextra)
  library(tidyverse)
  library(TSclust)
  library(cluster)
  ## cluster 01
  # Hierarchical clustering
  # Test del mejor método 
  # methods to assess
  # Hallar distancia
  st <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2021,12), by = "month")))
  colnames(Fechas) <- c("Fechas") ## Creo columna de formato fechas en tipo dataframe
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  ## Analisis del grupo 1 
  # Transpone todas las columnas menos la primer
  Grupo1 <- cbind(Fechas,df_transpose)
  
  # Añadimos los nombres de las columnas
  library(feasts)
  library(forecast)
  library(reshape2)
  library(fable)
  library(tsibble)
  library(lubridate)
  st <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2021,12), by = "month")))
  colnames(Fechas) <- c("Fechas") ## Creo columna de formato fechas en tipo dataframe
  
  
  ## Analisis del grupo 1 
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo1)>= 3) {
    Grupo1$Seriestotal <- rowSums(Grupo1[,2:length(Grupo1)])
  } else{
    Grupo1$Seriestotal <- Grupo1[,2]
  }
  
  
  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo1$Seriestotal[1:84],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo1$Seriestotal[1:84],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo1$Seriestotal[1:84],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  f <- c("1")
  Nombres_coef <- c("a","b", "s1", "s2","s3", "sse")
  for (c in Nombres_coef[1:5]) {
    a <- try(serie$coefficients[[c]], silent=TRUE)
    if ('try-error' %in% class(a)){
      f <- c(f,0)
    }else{
      f <- c(f, serie$coefficients[[c]])
    }
  }
  
  f <- c(f,serie$SSE)
  al <- try(serie$alpha[["alpha"]], silent=TRUE)
  if ('try-error' %in% class(al)){
    f <- c(f,0)
    #llenar coeficiente gamma
  }else{
    f <- c(f,serie$alpha[["alpha"]])
  }
  #llenar coeficiente alpha
  be <- try(serie$beta[["beta"]], silent=TRUE)
  if ('try-error' %in% class(be)){
    f <- c(f,0)
  }else{
    f <- c(f, serie$beta[["beta"]])
  }
  #llenar coeficiente beta
  ta <- try(serie$gamma[["gamma"]], silent=TRUE)
  if ('try-error' %in% class(ta)){
    f <- c(f,0)
  }else{
    f <- c(f, serie$gamma[["gamma"]])
  }
  st <- cbind(st,fore$upper[,1])
  
  ## Analisis del grupo 2 
  
  
  colnames(st) <- c("Mes", "Prediccion_Grupo1")
  ## MAPE
  mape <- c()
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo1,Grupo1$Seriestotal[73:84]))
  library(dplyr)
  mapes <- mape
  mape <- c("MAPE", mape)
  names(mape) <- c("Indicador", "Prediccion_Grupo1") 
  st$Total <- st[,2]
  PxF <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  RxF <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  # Transpone todas las columnas menos la primer
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  df_transpose$Total <- df_transpose[,1]
  RxF <- cbind(RxF,df_transpose$Total[73:84])
  PxF <- cbind(PxF,st$Total)
  f <- c(f,mapes)
  carga <- c(carga, st[,2])
  return(list(Pronosticos = st , Carg = carga, MAPE = mape, Paramet = f))
}

Cluster4 <- function(n1,carga){
  library(factoextra)
  library(tidyverse)
  library(TSclust)
  library(cluster)
  ## cluster 01
  # Hierarchical clustering
  # Test del mejor método 
  # methods to assess
  # Hallar distancia
  st <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2021,12), by = "month")))
  colnames(Fechas) <- c("Fechas") ## Creo columna de formato fechas en tipo dataframe
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  ## Analisis del grupo 1 
  # Transpone todas las columnas menos la primer
  Grupo1 <- cbind(Fechas,df_transpose)
  
  # Añadimos los nombres de las columnas
  library(feasts)
  library(forecast)
  library(reshape2)
  library(fable)
  library(tsibble)
  library(lubridate)
  st <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2021,12), by = "month")))
  colnames(Fechas) <- c("Fechas") ## Creo columna de formato fechas en tipo dataframe
  
  
  ## Analisis del grupo 1 
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo1)>= 3) {
    Grupo1$Seriestotal <- rowSums(Grupo1[,2:length(Grupo1)])
  } else{
    Grupo1$Seriestotal <- Grupo1[,2]
  }
  
  
  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo1$Seriestotal[1:84],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo1$Seriestotal[1:84],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo1$Seriestotal[1:84],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- auto.arima(ls1)
  fore <- forecast(serie,h = 12)
  st <- cbind(st,fore$mean)
  
  ## Analisis del grupo 2 
  
  
  colnames(st) <- c("Mes", "Prediccion_Grupo1")
  ## MAPE
  mape <- c()
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo1,Grupo1$Seriestotal[73:84]))
  library(dplyr)
  
  mape <- c("MAPE", mape)
  names(mape) <- c("Indicador", "Prediccion_Grupo1") 
  st$Total <- st[,2]
  PxF <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  RxF <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  # Transpone todas las columnas menos la primer
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  df_transpose$Total <- df_transpose[,1]
  RxF <- cbind(RxF,df_transpose$Total[73:84])
  PxF <- cbind(PxF,st$Total)
  
  carga <- c(carga, st[,2])
  
  return(list(Pronosticos = st , Carg = carga, MAPE = mape))
}