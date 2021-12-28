## Funciones Necesarias.
## Grafico de mapas Dinamicos de una tabla de Frecuencia.

Grafico_D <- function(DatxProv)
{
  ORDEN1 <- data.frame(DatxProv)
  library(h2o)
  library(dplyr) ## Manejo de data en los mapas
  library(highcharter)
  library(rjson)
  # data : lista de frecuencias de delitos a nivel provincial 
  ECUADOR <- fromJSON(file = "https://raw.githubusercontent.com/Rusersgroup/mapa_ecuador/master/ec-all.geo.json")
  mapdata <- get_data_from_map(download_map_data("countries/ec/ec-all")) ## Mapa del ecuador en datos
  ORDEN2 <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(mapdata$name)) ## Pone mayusculas a todos los elementos del data frame
  X <- c()
  for (i in 1:(length(ORDEN2)-1)) {
    for (j in 1:dim(ORDEN1)[1]) {
      if (ORDEN2[i] == toString(ORDEN1$Provincias[j]) ) {
        X <- c(X,ORDEN1[j,2])
      }
  }
  }
  data_ecuador <- mapdata %>% select(PROVINCIA = `name`) %>% mutate( X = c(X,0))
  data_ecuador[7,1] <- 	"Santo Domingo De Los Tsachilas"
  data_ecuador[24,1] <- "Galapagos"
  return(data_ecuador)
}


## Funciones a nivel Provincial.
Frec_Prov <- function(DXSiaf){
  library(dplyr)
  DatProv <- as.data.frame(table(DXSiaf$Provincia)) ## Creamos una tabla con la frecuencia de datos de cada provincia.
  names(DatProv) <- c("Provincias", "Frecuencia") ## Renombramos las columnas.
  DatProv <- arrange(DatProv, desc(Frecuencia))
  return(DatProv)
}
Datos_de_p  <- function(DXSiaf,Provincia){
 PROVINCIA <- DXSiaf[DXSiaf$Provincia == Provincia , ]
  return(PROVINCIA)
}

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

Datos_de_c  <- function(PROVINCIA, Canton){
  CANTON <- PROVINCIA[PROVINCIA$`CANTÓN DEL INCIDENTE` == Canton , ]
  return(CANTON)
}

A15_20 <-  function(CANTON,Tipos_d){
  library(stringr)
  
  CANTON <- CANTON[ ,c("Delito", "Fecha_Incidente")]
  CANTON$AÑO <- format(as.Date(CANTON$Fecha_Incidente), '%Y')
  CANTON$MES <- format(as.Date(CANTON$Fecha_Incidente), '%m')
  
  
  QxM <- data.frame() ## LLENAREMOS DE DELITOS POR MES 
  QxA <- data.frame() ## LLENAREMOS DE DELITOS POR AÑO 
  
  AÑOS <- c("2015","2016","2017","2018","2019","2020") ## AÑOS EN ESTUDIO(  i)
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
      QxA <-  rbind(QxA, c(Tipos_d[d],rep(0,6)))
      QxM <-  rbind(QxM, c(Tipos_d[d],rep(0,6*12)))
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


## Asignacion segun catalogo de delitos

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

# Analisis y pronóstico de series tempo 

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
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2020,12), by = "month")))
  colnames(Fechas) <- c("Fechas")
  df_transpose <- cbind.data.frame(Fechas, df_transpose)
  # Dissimilarity matrix
  if (length(df_transpose)>= 2.1) {
    DxA <- as.data.frame(colSums(df_transpose[1:60,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c()
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n1[,61:73])))} else{
      
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
    DxA <- as.data.frame(colSums(df_transpose[1:60,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n2 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n2[,61:73])))} else{
      
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
    DxA <- as.data.frame(colSums(df_transpose[1:60,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n3 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n3[,61:73])))} else{
      
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
    DxA <- as.data.frame(colSums(df_transpose[1:60,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n4 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n4[,61:73])))} else{
      
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
    DxA <- as.data.frame(colSums(df_transpose[1:60,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n5 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n5[,61:73])))} else{
      
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
    DxA <- as.data.frame(colSums(df_transpose[1:60,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n6 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n6[,61:73])))} else{
      
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
    DxA <- as.data.frame(colSums(df_transpose[1:60,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n7 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n7[,61:73]))) } else{
      
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
    DxA <- as.data.frame(colSums(df_transpose[1:60,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n8 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n8[,61:73])))} else{
      
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
    DxA <- as.data.frame(colSums(df_transpose[1:60,2:length(df_transpose)]))
    DxA <- cbind(rownames(DxA),DxA)
    colnames(DxA) <- c("Tipo de Delito","cantidad")
    Unicos <- DxA[DxA$cantidad > 2.2,] ## Delitos que ocurren una vez cada 5 años
    ### Delitos que ocurrieron pocas veces 
    Unicos <- as.data.frame(Unicos[,1])
    colnames(Unicos) <- c("Tipo de Delito")
    n9 <- merge(n1, Unicos, by = c("Tipo de Delito"))
    #### Hallar la correlacion de los delitos.
    Datos_Reales <- c(Datos_Reales, sum(rowSums(n9[,61:73])))} else{
  
      n9 <- data.frame()
    }
  
  
  return(list(DELFISC1 = n, DELFISC2 = n2, DELFISC3 = n3, DELFISC4 = n4, DELFISC5 = n5, DELFISC6 = n6, DELFISC7 = n7, DELFISC8 = n8, DELFISC9 = n9, Datos_Real = Datos_Reales))
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
  agnes(n1[,2:72], method = "ward", metric = "manhattan")$ac
  
  # Hallar distancia 
  d <- dist(n1[,2:72], method = "manhattan")
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
  g1 <- merge(clust1,n1[,1:73], by = c("Tipo de Delito"))
  clust2 <- as.data.frame(clust[clust$Grupo == 2 , ]$`Tipo de Delito`)
  colnames(clust2) <- c("Tipo de Delito")
  g2 <- merge(clust2,n1[,1:73], by = c("Tipo de Delito"))
  clust3 <- as.data.frame(clust[clust$Grupo ==  3 , ]$`Tipo de Delito`)
  colnames(clust3) <- c("Tipo de Delito")
  g3 <- merge(clust3,n1[,1:73], by = c("Tipo de Delito"))
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
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2020,12), by = "month")))
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
  nsdiffs(ts(Grupo1$Seriestotal[1:60],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo1$Seriestotal[1:60],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo1$Seriestotal[1:60],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  st <- cbind(st,fore$upper[,1])
  
  ## Analisis del grupo 2 
  
  Grupo2 <- cbind(Fechas,Grupo2[1:72,]) ## Uno los datos con las fechas
  
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo2)>= 3) {
    Grupo2$Seriestotal <- rowSums(Grupo2[,2:length(Grupo2)])
  } else{
    Grupo2$Seriestotal <- Grupo2[,2]
  }

  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo2$Seriestotal[1:60],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo2$Seriestotal[1:60],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo2$Seriestotal[1:60],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  st <- cbind(st,fore$upper[,1])
  
  ## Análisis del grupo 3 de la fiscalía 
 
  ## Analisis del grupo 3
  
  Grupo3 <- cbind(Fechas,Grupo3[1:72,]) ## Uno los datos con las fechas
  
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo3)>= 3) {
    Grupo3$Seriestotal <- rowSums(Grupo3[,2:length(Grupo3)])
  } else{
    Grupo3$Seriestotal <- Grupo3[,2]
  }
 
  ## Descomposicion de la serie.
  comp <- decompose(ts(Grupo3$Seriestotal[1:60],frequency = 12)) ## Componentes de la serie 

  
  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo3$Seriestotal[1:60],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo3$Seriestotal[1:60],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo3$Seriestotal[1:60],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  st <- cbind(st,c(fore$mean))
  
  colnames(st) <- c("Mes", "Prediccion_Grupo1", "Prediccion_Grupo2", "Prediccion_Grupo3")
  ## MAPE
  mape <- c()
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo1,Grupo1$Seriestotal[61:72]))
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo2,Grupo2$Seriestotal[61:72]))
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo3,Grupo3$Seriestotal[61:72]))
  library(dplyr)
  
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
  RxF <- cbind(RxF,df_transpose$Total[61:72])
  PxF <- cbind(PxF,st$Total)
  
  carga <- c(carga, ceiling(sum(colSums(st[,2:(length(st)-1)]))))
  
  return(list(Pronosticos = st , Carg = carga, MAPE = mape))
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
  agnes(n1[,2:72], method = "ward", metric = "manhattan")$ac
  
  # Hallar distancia 
  d <- dist(n1[,2:72], method = "manhattan")
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
  g1 <- merge(clust1,n1[,1:73], by = c("Tipo de Delito"))
  clust2 <- as.data.frame(clust[clust$Grupo == 2 , ]$`Tipo de Delito`)
  colnames(clust2) <- c("Tipo de Delito")
  g2 <- merge(clust2,n1[,1:73], by = c("Tipo de Delito"))
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
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2020,12), by = "month")))
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
  nsdiffs(ts(Grupo1$Seriestotal[1:60],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo1$Seriestotal[1:60],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo1$Seriestotal[1:60],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  st <- cbind(st,c(fore$mean))
  
  ## Analisis del grupo 2 
  
  Grupo2 <- cbind(Fechas,Grupo2[1:72,]) ## Uno los datos con las fechas
  
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo2)>= 3) {
    Grupo2$Seriestotal <- rowSums(Grupo2[,2:length(Grupo2)])
  } else{
    Grupo2$Seriestotal <- Grupo2[,2]
  }
  
  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo2$Seriestotal[1:60],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo2$Seriestotal[1:60],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo2$Seriestotal[1:60],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  st <- cbind(st,fore$upper[,2])
  
  colnames(st) <- c("Mes", "Prediccion_Grupo1", "Prediccion_Grupo2")
  ## MAPE
  mape <- c()
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo1,Grupo1$Seriestotal[61:72]))
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo2,Grupo2$Seriestotal[61:72]))
  library(dplyr)
  
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
  RxF <- cbind(RxF,df_transpose$Total[61:72])
  PxF <- cbind(PxF,st$Total)
  
  carga <- c(carga, ceiling(sum(colSums(st[,2:(length(st)-1)]))))
  
  return(list(Pronosticos = st , Carg = carga, MAPE = mape))
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
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2020,12), by = "month")))
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
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2020,12), by = "month")))
  colnames(Fechas) <- c("Fechas") ## Creo columna de formato fechas en tipo dataframe
  
  
  ## Analisis del grupo 1 
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo1)>= 3) {
    Grupo1$Seriestotal <- rowSums(Grupo1[,2:length(Grupo1)])
  } else{
    Grupo1$Seriestotal <- Grupo1[,2]
  }
  
  
  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo1$Seriestotal[1:60],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo1$Seriestotal[1:60],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo1$Seriestotal[1:60],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  st <- cbind(st,fore$upper[,1])
  
  ## Analisis del grupo 2 
  
  
  colnames(st) <- c("Mes", "Prediccion_Grupo1")
  ## MAPE
  mape <- c()
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo1,Grupo1$Seriestotal[61:72]))
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
  RxF <- cbind(RxF,df_transpose$Total[61:72])
  PxF <- cbind(PxF,st$Total)
  
  carga <- c(carga, st[,2])
  
  return(list(Pronosticos = st , Carg = carga, MAPE = mape))
}

Cluster1 <- function(n1,carga){
  
  library(feasts)
  library(forecast)
  library(reshape2)
  library(fable)
  library(tsibble)
  library(lubridate)
  st <- data.frame(c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago", "Sep", "Oct", "Nov", "Dic"))
  Fechas <- as.data.frame(yearmonth(seq.Date(from = make_date(2015,01), to = make_date(2020,12), by = "month")))
  colnames(Fechas) <- c("Fechas") ## Creo columna de formato fechas en tipo dataframe
  df_transpose <- data.frame(t(n1[-1]))
  # Añadimos los nombres de las columnas
  colnames(df_transpose) <- n1[, 1]
  ## Analisis del grupo 1 
  
  Grupo1 <- cbind(Fechas,df_transpose[,1])## Uno los datos con las fechas
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo1)>= 3) {
    Grupo1$Seriestotal <- rowSums(Grupo1[,2:length(Grupo1)])
  } else{
    Grupo1$Seriestotal <- Grupo1[,2]
  }

  ## Descomposicion de la serie.
  comp <- decompose(ts(Grupo1$Seriestotal[1:60],frequency = 12)) ## Componentes de la serie 

  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo1$Seriestotal[1:60],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo1$Seriestotal[1:60],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo1$Seriestotal[1:60],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  st <- cbind(st,fore$mean)
  
  ## Analisis del grupo 2 
  
  Grupo2 <- cbind(Fechas ,df_transpose[,2])## Uno los datos con las fechas
  
  ## Serie de total de delitos por mes en el Grupo 1
  if (length(Grupo2)>= 3) {
    Grupo2$Seriestotal <- rowSums(Grupo2[,2:length(Grupo2)])
  } else{
    Grupo2$Seriestotal <- Grupo2[,2]
  }
  ## columna total que suma los delitos por mes 

  ## Descomposicion de la serie.
  comp <- decompose(ts(Grupo2$Seriestotal[1:60],frequency = 12)) ## Componentes de la serie 

  ## Diferenciar por estacionalidad o estacionariedad.
  nsdiffs(ts(Grupo2$Seriestotal[1:60],frequency = 12)) ## diferenciaciones 
  ndiffs(ts(Grupo2$Seriestotal[1:60],frequency = 12)) ## por tendencia
  
  ## Logaritmos o diferenciaciones
  ls1 <- ts(Grupo2$Seriestotal[1:60],frequency = 12)
  
  ## creacion del modelo Holtwinters para el total
  serie <- HoltWinters(ls1, seasonal = "additive")
  fore <- forecast:::forecast.HoltWinters(serie, h=12)
  st <- cbind(st,fore$upper[,1])
  
  colnames(st) <- c("Mes", "Prediccion_Grupo1", "Prediccion_Grupo2")
  ## MAPE
  mape <- c()
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo1,Grupo1$Seriestotal[61:72]))
  mape <- c(mape, MLmetrics::MAPE(st$Prediccion_Grupo2,Grupo2$Seriestotal[61:72]))
  library(dplyr)
  
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
  RxF <- cbind(RxF,df_transpose$Total[61:72])
  PxF <- cbind(PxF,st$Total)
  
  carga <- c(carga, ceiling(sum(colSums(st[,2:(length(st)-1)]))))
  
  return(list(Pronosticos = st , Carg = carga, MAPE = mape))
  
}
