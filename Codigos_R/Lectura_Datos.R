##**************** Datos a nivel cantonal *************************###
## Esta es la función que retorna los datos de cada provincia - canton usando 
## Catálogo de delitos COIP.
## Delitos ESTUDIO-EPN (Delitos del SIAF corregidos).

library(readxl) ## Lectura de excel.

###### BASE Catálogo de delitos COIP.
DXCat <- read_excel("Catalogo_de_delitos.xlsx", 
                  sheet = "CATALOGO DE DELITOS COIP") ## Delitos segun catalogo.
# Arreglo los datos que tienen signos demás.
## Catálogos
DXCat$DELITO = chartr("ÁÉÍÓÚ", "AEIOU", toupper(DXCat$DELITO)) ## Quitar tildes
Asignacion <- as.data.frame(cbind(DXCat$DELITO, DXCat$ESPECIALIZADA))
Asignacion <- Asignacion[!duplicated(Asignacion), ]
names(Asignacion) <- c("Delito","Fiscalía")

###### BASE Registros de delitos del COIP corregidos.
## Librerias para bases en SQL.
library(pool)
pool <- dbPool(drv = RMySQL::MySQL(), dbname = 'delitos', host = "localhost", username = "root", password = 'nmintmls', port = 3306)
DXSiaf <- dbGetQuery(pool, "SELECT * FROM tmp_epn_estudio;")


# Selecciono las columnas que usaré en el análisis.
DXSiaf <- DXSiaf[,c(1,2,3,6,7,8,9,11,15)]

# Tomo datos del 2015-2020 que son los que tienen la informacíon completa.
DXSiaf <- DXSiaf[format(as.Date(DXSiaf$Fecha_Incidente), '%Y') == "2015" | format(as.Date(DXSiaf$Fecha_Incidente), '%Y') == "2016" | format(as.Date(DXSiaf$Fecha_Incidente), '%Y') == "2017" | format(as.Date(DXSiaf$Fecha_Incidente), '%Y') == "2018" | format(as.Date(DXSiaf$Fecha_Incidente), '%Y') == "2019" | format(as.Date(DXSiaf$Fecha_Incidente), '%Y') == "2020",] 

# Elimino Valores Nulos.
DXSiaf <- DXSiaf[is.na(DXSiaf$Provincia) == FALSE , ] # 2Minutos


rm(DXCat,pool)


# SIAF
DXSiaf$Delito = chartr("ÁÉÍÓÚ", "AEIOU", toupper(DXSiaf$Delito)) ## Quitar Tildes
DXSiaf$Delito[DXSiaf$Delito == "CONTRAVENCION DE HURTO"] = "HURTO"
DXSiaf$Delito[DXSiaf$Delito == "DAÑOS MATERIALES\r\n"] = "DAÑOS MATERIALES"
DXSiaf$Delito[DXSiaf$Delito == "DESAPARICION INVOLUNTARIA. "] = "DESAPARICION INVOLUNTARIA."
DXSiaf$Delito[DXSiaf$Delito == "FALSEDAD DE CONTENIDO EN RECETAS, EXAMENES O CERTIFICADOS MEDICOS.- "] = "FALSEDAD DE CONTENIDO EN RECETAS, EXAMENES O CERTIFICADOS MEDICOS.-"
DXSiaf$Delito[DXSiaf$Delito == "FALSIFICACION DE MARCAS Y PIRATERIA LESIVA CONTRA LOS DERECHOS DE AUTOR\r\n" ] = "FALSIFICACION DE MARCAS Y PIRATERIA LESIVA CONTRA LOS DERECHOS DE AUTOR"
DXSiaf[DXSiaf$Provincia == "CANAR",]$Provincia <- "CAÑAR"
## Eliminar las fiscalías que tienen fiscales específicos FISCALIA DE FUERO PROVINCIAL
DXSiaf <- DXSiaf[DXSiaf$Fiscalia_Especializada != "FISCALIA DE FUERO NACIONAL",]
DXSiaf <- DXSiaf[DXSiaf$Fiscalia_Especializada != "FISCALIA DE FUERO PROVINCIAL",]
DXSiaf <- DXSiaf[DXSiaf$Fiscalia_Especializada != "FISCALIA DE FUERO NACIONAL - FISCAL SUBROGANTE",]
DXSiaf <- DXSiaf[DXSiaf$Fiscalia_Especializada != "UNIDAD DE INVESTIGACIÓN DE DELITOS TRIBUTARIOS Y ADUANEROS",]
#DXSiaf <- DXSiaf[DXSiaf$Fiscalia_Especializada != "UNIDAD DE INVESTIGACIÓN DE DELITOS TRIBUTARIOS Y ADUANEROS",]
### Se tienen las bases limpias para el estudio (Catalogo, DxSiaf)

