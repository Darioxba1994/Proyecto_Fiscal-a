# *************** Hallar la tasa de carga de delitos  por fiscal.*************

# 1. Dividiremos la base en 2 (Delitos Flagrantes | Delitos no flarantes) "aplicar en predicciones" 
library(readxl) # Libreria para cargar datos de Excel a R
CARGA_LABORAL <- read_excel("CARGA LABORAL.xlsx") # Lectura de base. Zamora Chinchipe (2015-2020) ""Provincia sin porblemas fiscales.
CARGA_LABORAL <- CARGA_LABORAL[is.na(CARGA_LABORAL$d_ANIO_REGISTRO) == FALSE , ]

#* Fiscales existentes 
Fiscales <- names(table(CARGA_LABORAL$d_CEDULA_FISCAL)) ## Vamos a identificar los fiscales de cada Canton de Zamora

#* Creacion de Bases con delitos Flagrantes y No_Flagrantes.
No_Flagrante <- CARGA_LABORAL[CARGA_LABORAL$d_TIPO == "No Flagrante", ]
Flagrante <- CARGA_LABORAL[CARGA_LABORAL$d_TIPO == "Flagrante",]

#* Casos de carga laboral por cada uno de los fiscales (Flagrantes)
#FECHA_CIERRE_IF
NAs <- Flagrante[is.na(Flagrante$f_FECHA_CIERRE_IF),]
SIN_NAs <- Flagrante[is.na(Flagrante$f_FECHA_CIERRE_IF) == FALSE ,]

#(SIN_NAs) CONTEOS DE DELITOS EN EL AÑO
AÑOSi <- data.frame(names(table(format(as.Date(SIN_NAs$f_FECHA_CIERRE_IF), '%Y'))), c(1:length(names(table(format(as.Date(SIN_NAs$f_FECHA_CIERRE_IF), '%Y')))))  )
colnames(AÑOSi) <- c("a1", "AÑO n°")
Fiscales_e <- c()
for (f in Fiscales) {
  Fis <- SIN_NAs[SIN_NAs$d_CEDULA_FISCAL == f , ]
  Fis1 <- NAs[NAs$d_CEDULA_FISCAL == f , ]
  Fis <- Fis[is.na(Fis$d_ANIO_REGISTRO) == FALSE,]
  Fis <- Fis[is.na(Fis$f_FECHA_CIERRE_IF) == FALSE,]
  Fis1 <- Fis1[is.na(Fis1$d_ANIO_REGISTRO) == FALSE,]
  Fis1 <- Fis1[is.na(Fis1$d_FECHA_ESTADO_PROCESAL) == FALSE,]
  a1 <- c()
  if(dim(Fis)[1] > 0 | dim(Fis1)[1] > 0) {
  if (dim(Fis)[1] > 0 ) {
    for (i in 1:dim(Fis)[1]) {
      a <- as.numeric(Fis[i,]$d_ANIO_REGISTRO)
      while (a <= as.numeric(format(as.Date(Fis[i,]$f_FECHA_CIERRE_IF), '%Y')))
      {
        a1 <- c(a1,a)
        a=a+1
      }
    }
  }
  if (dim(Fis1)[1] > 0) {
    for (i in 1:dim(Fis1)[1]) {
      a <- as.numeric(Fis1[i,]$d_ANIO_REGISTRO)
      if (Fis1[i,]$d_FECHA_ESTADO_PROCESAL != "0000-00-00 00:00:00") {
        while (a < as.numeric(format(as.Date(Fis1[i,]$d_FECHA_ESTADO_PROCESAL), '%Y')) )
        {
          a1 <- c(a1,a)
          a=a+1
        }
      }
      
    }
  }
    Fiscales_e <- c(Fiscales_e,f) 
    AÑOS <- as.data.frame(table(a1))
    AÑOSi <- merge(AÑOSi, AÑOS, by = "a1" , all.x = TRUE)
  }
}

colnames(AÑOSi) <- c("AÑO", "Nro" ,  Fiscales_e) ## Delitos por año para cada fiscal 
AÑOSi[is.na(AÑOSi)] <- 0
AÑOSi <- AÑOSi[2:6,]


# Para delitos No Flagrantes
NAs <- No_Flagrante[is.na(No_Flagrante$f_FECHA_CIERRE_IF) == TRUE,]
SIN_NAs <- No_Flagrante[is.na(No_Flagrante$f_FECHA_CIERRE_IF) == FALSE ,]

AÑOSi1 <- data.frame(names(table(format(as.Date(SIN_NAs$f_FECHA_CIERRE_IF), '%Y'))), c(1:length(names(table(format(as.Date(SIN_NAs$f_FECHA_CIERRE_IF), '%Y')))))  )
colnames(AÑOSi1) <- c("a1", "AÑO n°")
Fiscales_e <- c()
for (f in Fiscales) {
  Fis <- SIN_NAs[SIN_NAs$d_CEDULA_FISCAL == f , ]
  Fis1 <- NAs[NAs$d_CEDULA_FISCAL == f , ]
  Fis <- Fis[is.na(Fis$d_ANIO_REGISTRO) == FALSE,]
  Fis <- Fis[is.na(Fis$f_FECHA_CIERRE_IF) == FALSE,]
  Fis1 <- Fis1[is.na(Fis1$d_ANIO_REGISTRO) == FALSE,]
  Fis1 <- Fis1[is.na(Fis1$d_FECHA_ESTADO_PROCESAL) == FALSE,]
  a1 <- c()
  if(dim(Fis)[1] > 0 | dim(Fis1)[1] > 0) {
    if (dim(Fis)[1] > 0 ) {
      for (i in 1:dim(Fis)[1]) {
        a <- as.numeric(Fis[i,]$d_ANIO_REGISTRO)
        while (a <= as.numeric(format(as.Date(Fis[i,]$f_FECHA_CIERRE_IF), '%Y')))
        {
          a1 <- c(a1,a)
          a=a+1
        }
      }
    }
    if (dim(Fis1)[1] > 0) {
      for (i in 1:dim(Fis1)[1]) {
        a <- as.numeric(Fis1[i,]$d_ANIO_REGISTRO)
        if (Fis1[i,]$d_FECHA_ESTADO_PROCESAL != "0000-00-00 00:00:00") {
          while (a < as.numeric(format(as.Date(Fis1[i,]$d_FECHA_ESTADO_PROCESAL), '%Y')) )
          {
            a1 <- c(a1,a)
            a=a+1
          }
        }
        
      }
    }
    Fiscales_e <- c(Fiscales_e,f) 
    AÑOS <- as.data.frame(table(a1))
    AÑOSi1 <- merge(AÑOSi1, AÑOS, by = "a1" , all.x = TRUE)
  }
}

colnames(AÑOSi1) <- c("AÑO", "Nro" ,  Fiscales_e) ## Delitos por año para cada fiscal 
AÑOSi1[is.na(AÑOSi1)] <- 0
AÑOSi1 <- AÑOSi1[3:7,c(1:4,6:length(AÑOSi1))]

library(xlsx)
write.xlsx(AÑOSi, file = "TASA_2.xlsx", sheetName = "Flagrantes")
write.xlsx(AÑOSi1, file = "TASA_2.xlsx", sheetName = "No_Flagrantes", append = T )

# Caracteristica de los Fiscales
NOFLAG <- c()
ARCHIV <- c()
Nro_CANT <- c()
Nro_FISC <- c()
## Usando la información de los fiscales encontrados vamos a definir carácterísticas.

CARGA_LAB1 <- CARGA_LABORAL[CARGA_LABORAL$d_CEDULA_FISCAL == "0701210437", ] ## Fiscal 1

NOFLAG <- c(NOFLAG, mean(table(CARGA_LAB1[CARGA_LAB1$d_TIPO == "No Flagrante",]$d_ANIO_REGISTRO)))
ARCHIV <- c(ARCHIV, mean(table(CARGA_LAB1[CARGA_LAB1$f_ESTADO_NDD == "ARCHIVADA POR ACEPTACION DE SOLICITUD",]$d_ANIO_REGISTRO)/table(CARGA_LAB1$d_ANIO_REGISTRO))*100)
Nro_CANT <- c(Nro_CANT, length(table(CARGA_LAB1$d_CANTON)))
Nro_FISC <- c(Nro_FISC, length(table(CARGA_LAB1$ESPECIALIZADA)))

CARGA_LAB2 <- CARGA_LABORAL[CARGA_LABORAL$d_CEDULA_FISCAL == "0703869990" , ] ## Fiscal 2

NOFLAG <- c(NOFLAG, mean(table(CARGA_LAB2[CARGA_LAB2$d_TIPO == "No Flagrante",]$d_ANIO_REGISTRO)))
ARCHIV <- c(ARCHIV, mean(table(CARGA_LAB2[CARGA_LAB2$f_ESTADO_NDD == "ARCHIVADA POR ACEPTACION DE SOLICITUD",]$d_ANIO_REGISTRO)/table(CARGA_LAB2$d_ANIO_REGISTRO))*100)
Nro_CANT <- c(Nro_CANT, length(table(CARGA_LAB2$d_CANTON)))
Nro_FISC <- c(Nro_FISC, length(table(CARGA_LAB2$ESPECIALIZADA)))

CARGA_LAB3 <- CARGA_LABORAL[CARGA_LABORAL$d_CEDULA_FISCAL == "1102922588" , ] ## Fiscal 3

NOFLAG <- c(NOFLAG, mean(table(CARGA_LAB3[CARGA_LAB3$d_TIPO == "No Flagrante",]$d_ANIO_REGISTRO)))
ARCHIV <- c(ARCHIV, mean(table(CARGA_LAB3[CARGA_LAB3$f_ESTADO_NDD == "ARCHIVADA POR ACEPTACION DE SOLICITUD",]$d_ANIO_REGISTRO)/table(CARGA_LAB3$d_ANIO_REGISTRO))*100)
Nro_CANT <- c(Nro_CANT, length(table(CARGA_LAB3$d_CANTON)))
Nro_FISC <- c(Nro_FISC, length(table(CARGA_LAB3$ESPECIALIZADA)))

CARGA_LAB4 <- CARGA_LABORAL[CARGA_LABORAL$d_CEDULA_FISCAL == "1103583595" , ] ## Fiscal 4

NOFLAG <- c(NOFLAG, mean(table(CARGA_LAB4[CARGA_LAB4$d_TIPO == "No Flagrante",]$d_ANIO_REGISTRO)))
ARCHIV <- c(ARCHIV, mean(table(CARGA_LAB4[CARGA_LAB4$f_ESTADO_NDD == "ARCHIVADA POR ACEPTACION DE SOLICITUD",]$d_ANIO_REGISTRO)/table(CARGA_LAB4$d_ANIO_REGISTRO))*100)
Nro_CANT <- c(Nro_CANT, length(table(CARGA_LAB4$d_CANTON)))
Nro_FISC <- c(Nro_FISC, length(table(CARGA_LAB4$ESPECIALIZADA)))

CARGA_LAB5 <- CARGA_LABORAL[CARGA_LABORAL$d_CEDULA_FISCAL == "1103865356" , ] ## Fiscal 5

NOFLAG <- c(NOFLAG, mean(table(CARGA_LAB5[CARGA_LAB5$d_TIPO == "No Flagrante",]$d_ANIO_REGISTRO)))
ARCHIV <- c(ARCHIV, mean(table(CARGA_LAB5[CARGA_LAB5$f_ESTADO_NDD == "ARCHIVADA POR ACEPTACION DE SOLICITUD",]$d_ANIO_REGISTRO)/table(CARGA_LAB5$d_ANIO_REGISTRO))*100)
Nro_CANT <- c(Nro_CANT, length(table(CARGA_LAB5$d_CANTON)))
Nro_FISC <- c(Nro_FISC, length(table(CARGA_LAB5$ESPECIALIZADA)))

CARGA_LAB6 <- CARGA_LABORAL[CARGA_LABORAL$d_CEDULA_FISCAL == "1103871966" , ] ## Fiscal 6

NOFLAG <- c(NOFLAG, mean(table(CARGA_LAB6[CARGA_LAB6$d_TIPO == "No Flagrante",]$d_ANIO_REGISTRO)))
ARCHIV <- c(ARCHIV, mean(table(CARGA_LAB6[CARGA_LAB6$f_ESTADO_NDD == "ARCHIVADA POR ACEPTACION DE SOLICITUD",]$d_ANIO_REGISTRO)/table(CARGA_LAB6$d_ANIO_REGISTRO))*100)
Nro_CANT <- c(Nro_CANT, length(table(CARGA_LAB6$d_CANTON)))
Nro_FISC <- c(Nro_FISC, length(table(CARGA_LAB6$ESPECIALIZADA)))

CARGA_LAB7 <- CARGA_LABORAL[CARGA_LABORAL$d_CEDULA_FISCAL == "1900159987" , ] ## Fiscal 7

NOFLAG <- c(NOFLAG, mean(table(CARGA_LAB7[CARGA_LAB7$d_TIPO == "No Flagrante",]$d_ANIO_REGISTRO)))
ARCHIV <- c(ARCHIV, mean(table(CARGA_LAB7[CARGA_LAB7$f_ESTADO_NDD == "ARCHIVADA POR ACEPTACION DE SOLICITUD",]$d_ANIO_REGISTRO)/table(CARGA_LAB7$d_ANIO_REGISTRO))*100)
Nro_CANT <- c(Nro_CANT, length(table(CARGA_LAB7$d_CANTON)))
Nro_FISC <- c(Nro_FISC, length(table(CARGA_LAB7$ESPECIALIZADA)))

CARGA_LAB8 <- CARGA_LABORAL[CARGA_LABORAL$d_CEDULA_FISCAL == "1900317072" , ] ## Fiscal 8

NOFLAG <- c(NOFLAG, mean(table(CARGA_LAB8[CARGA_LAB8$d_TIPO == "No Flagrante",]$d_ANIO_REGISTRO)))
ARCHIV <- c(ARCHIV, mean(table(CARGA_LAB8[CARGA_LAB8$f_ESTADO_NDD == "ARCHIVADA POR ACEPTACION DE SOLICITUD",]$d_ANIO_REGISTRO)/table(CARGA_LAB8$d_ANIO_REGISTRO))*100)
Nro_CANT <- c(Nro_CANT, length(table(CARGA_LAB8$d_CANTON)))
Nro_FISC <- c(Nro_FISC, length(table(CARGA_LAB8$ESPECIALIZADA)))

CARGA_LAB9 <- CARGA_LABORAL[CARGA_LABORAL$d_CEDULA_FISCAL == "1900334606" , ] ## Fiscal 9

NOFLAG <- c(NOFLAG, mean(table(CARGA_LAB9[CARGA_LAB9$d_TIPO == "No Flagrante",]$d_ANIO_REGISTRO)))
ARCHIV <- c(ARCHIV, mean(table(CARGA_LAB9[CARGA_LAB9$f_ESTADO_NDD == "ARCHIVADA POR ACEPTACION DE SOLICITUD",]$d_ANIO_REGISTRO)/table(CARGA_LAB9$d_ANIO_REGISTRO))*100)
Nro_CANT <- c(Nro_CANT, length(table(CARGA_LAB9$d_CANTON)))
Nro_FISC <- c(Nro_FISC, length(table(CARGA_LAB9$ESPECIALIZADA)))

Ind <- cbind( c("0701210437","0703869990","1102922588", "1103583595","1103865356","1103871966","1900159987","1900317072","1900334606"), ARCHIV, Nro_CANT,Nro_FISC )

# ** caracteristicas del fiscal.
colnames(Ind) <- c("Fiscales", "Archivados", "Cantones", "Fiscalías")

write.xlsx(Ind, file = "TASA_2.xlsx", sheetName = "Archivados", append = T )

rm(Nro_FISC,Nro_CANT,ARCHIV,NOFLAG,CARGA_LAB9,CARGA_LAB8,CARGA_LAB7,CARGA_LAB6,CARGA_LAB5,CARGA_LAB4,CARGA_LAB3,CARGA_LAB2,CARGA_LAB1)
rm(AÑOS,Fis,Fis1,Flagrante, No_Flagrante,NAs,SIN_NAs,CARGA_LABORAL)
rm(a,a1,f,Fiscales,Fiscales_e,i)