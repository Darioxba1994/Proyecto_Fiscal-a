### Datos a nivel Provincial y Cantonal - union de todos los Scripts. ###
# En este estudio vamos a tratar de obtener la información pronosticada a nivel provincial-cantonal.
library(kableExtra)
# 1. Lectura de Datos
source("Lectura_Datos.R") ## 2 minutos en leer archivos 
source("funciones.R") ## Funciones para resultados

# 1.1 Resultados a nivel nacional
library(kableExtra) ## Libreria para mostrar tablas

DatxProv <- Frec_Prov(DXSiaf)
kbl(DatxProv) %>% kable_styling() ## Mostrar tabla de datos por provincia.
data_ecuador <- Grafico_D(DatxProv) ## Datos para graficos de ECUADOR

## Funcion que grafica el mapa del Ecuador.
ecuador <- rjson::fromJSON( file = "https://raw.githubusercontent.com/Rusersgroup/mapa_ecuador/master/ec-all.geo.json") ## DATOS DE ECUADOR
highchart() %>%
       hc_title(text = "<i>Mapa dinámico de Ecuador</i>",
                margin = 20, align = "center",
                style = list(color = "#08338F", useHTML = TRUE)) %>% 
       hc_subtitle(text = "Delitos por provincia",
                   align = "center",
                   style = list(color = "#0C5C9E", fontWeight = "bold")) %>% 
       hc_tooltip(followPointer =  FALSE) %>%
       hc_add_series_map(ecuador, data_ecuador, name = "Delitos",
                         value = "X", joinBy = c("name", "PROVINCIA"),
                         dataLabels = list(enabled = TRUE,
                                           format = '{point.properties.woe-name}')) %>%
       hc_colorAxis(minColor = "yellow", maxColor = "red")  %>%
       hc_legend(align = "center", x = 0, y = -70) %>%
       hc_mapNavigation(enabled = TRUE) %>%
       hc_add_theme(hc_theme_ffx()) %>% 
       hc_add_annotation(xValue = 0, yValue = 0, title = list(text = 'Fuente: INEC')) %>% 
       hc_chart(borderColor = "#08338F",
                borderRadius = 10,
                borderWidth = 2)


## Grafico del histogrma.
barplot(DatxProv$Frecuencia, names.arg = DatxProv$Provincias, ylab = "Cantidad de Delitos", main = "Cantidad de Delitos por Provincia ",las = 3) 


# 1.2 Resultados a nivel Provincial.


Provincia <- "SANTA ELENA"
PROVINCIA <- Datos_de_p(DXSiaf,Provincia) # Elijo data de la Provincia en estudio.
## Datos para el mapa de la provincia.
Frec <- Grafico_P(Provincia,PROVINCIA) ## Datos de frecuecnia en orde de la data para mapa
kbl(Frec$NOMBRES) %>% kable_styling()

## GRAFICO CANTONAL DE LA PROVINCIA ELEGIDA.
library(sf)
cantones <- st_read("ecuador.json")
PROVINCIA1 <- cantones[cantones$DPA_DESPRO == chartr("Ñ", "Ð", toupper(Provincia)) ,]

ggplot(data = PROVINCIA1) +
  geom_sf(aes(fill = Frec$CANTIDAD ))+ labs(title = "",
                                   fill = "Delitos")+ geom_text(data = Frec$COORDE, aes(X, Y, label = canton), colour = "black", size=3) + scale_fill_gradient(low="yellow", high="red") 

## HISTOGRAMA DE LA SITUACIÓN DE LOS DATOS DE LA PROVINCIA
barplot(Frec$NOMBRES$Frecuencia,names.arg = Frec$NOMBRES$Nom_Can,  ylab = "Cantidad de Delitos", main = "Cantidad de Delitos por Cantón",las = 0.5) 

SANTA_ELENA <- c( names(table(PROVINCIA$Canton)))
print(SANTA_ELENA)

## Borrar informacion innecesarias 
rm(DXSiaf,cantones,PROVINCIA1)

# 1.3 Resultados a Nivel Cantonal depende de su provincia.
Canton <- "LA LIBERTAD"
CANTON <- Datos_de_c(PROVINCIA,Canton) ## Data a nivel Cantonal.
dim(CANTON)[1] ## numero de delitos en el cantón


# 1.3.1 Obtener las series de tiempo para el análisis de pronósticos del Cantón.
Tipos_d <- names(table(PROVINCIA$Delito))
Series <- A15_20(CANTON,Tipos_d)
kbl(Series$DelxMes) %>% kable_styling()
kbl(Series$DelxAño) %>% kable_styling()


library(dplyr)
# 1.3.2 Análisis y Pronostico de Series
## Asignacion de fiscalias y delitos segun catalogo
Fiscalias <- Asig_Fisc(Asignacion)
Ser <- data.frame(Series[1])
Ser[,2:length(Ser)] <- as.numeric(unlist(Ser[,2:length(Ser)])) ## cambio a terminos numericos las series 
names(Ser)[1] <- "Tipo de Delito"
Resultados <- Analisis(Ser,Asignacion)

kbl(Resultados$DELFISC5) %>% kable_styling()

##DELITOS EN EL CANTON
dim(Resultados$DELFISC1)[1]
dim(Resultados$DELFISC2)[1]
dim(Resultados$DELFISC3)[1]
dim(Resultados$DELFISC4)[1]
dim(Resultados$DELFISC5)[1]
dim(Resultados$DELFISC6)[1]
dim(Resultados$DELFISC7)[1]
dim(Resultados$DELFISC8)[1]
dim(Resultados$DELFISC9)[1]

# 1.4. Datos en formato python para el modelo.
carga <- c() ## CARGA ASIGNADA A LA FISCALÍA.
rm(F1,F2,F3,F4,F5,F6,F7,F8,F9)
F1 <- Cluster(Resultados$DELFISC1,carga)# Resltados para fiscalías con pocos delitos.
F2 <- Cluster1(Resultados$DELFISC2,carga)
F3 <- Cluster2(Resultados$DELFISC3,carga)
F4 <- Cluster(Resultados$DELFISC4,carga)
F5 <- Cluster(Resultados$DELFISC5,carga)
F6 <- Cluster(Resultados$DELFISC6,carga)
F7 <- Cluster(Resultados$DELFISC7,carga)
F8 <- Cluster(Resultados$DELFISC8,carga)
F9 <- Cluster(Resultados$DELFISC9,carga) 

## Reultados de la funcion cluster
kbl(F5$Pronosticos) %>% kable_styling()

### Guardar datos de cada Canton

library(writexl)
library(xlsx)
rm(r1,r2,r3,r4,r5,r6,r7,r8,r9)
r1 <- c("Fiscalia 1" , sum(floor(F1$Pronosticos$Prediccion_Grupo1)), floor(sum(F1$Pronosticos$Prediccion_Grupo2)), floor(sum(F1$Pronosticos$Prediccion_Grupo3)))
r2 <- c("Fiscalia 2" , sum(floor(F2$Pronosticos$Prediccion_Grupo1)), floor(sum(F2$Pronosticos$Prediccion_Grupo2)), floor(sum(F2$Pronosticos$Prediccion_Grupo3)))
r3 <- c("Fiscalia 3" , sum(floor(F3$Pronosticos$Prediccion_Grupo1)), floor(sum(F3$Pronosticos$Prediccion_Grupo2)), floor(sum(F3$Pronosticos$Prediccion_Grupo3)))
r4 <- c("Fiscalia 4" , sum(floor(F4$Pronosticos$Prediccion_Grupo1)), floor(sum(F4$Pronosticos$Prediccion_Grupo2)), floor(sum(F4$Pronosticos$Prediccion_Grupo3)))
r5 <- c("Fiscalia 5" , sum(floor(F5$Pronosticos$Prediccion_Grupo1)), floor(sum(F5$Pronosticos$Prediccion_Grupo2)), floor(sum(F5$Pronosticos$Prediccion_Grupo3)))
r6 <- c("Fiscalia 6" , sum(floor(F6$Pronosticos$Prediccion_Grupo1)), floor(sum(F6$Pronosticos$Prediccion_Grupo2)), floor(sum(F6$Pronosticos$Prediccion_Grupo3)))
r7 <- c("Fiscalia 7" , sum(floor(F7$Pronosticos$Prediccion_Grupo1)), floor(sum(F7$Pronosticos$Prediccion_Grupo2)), floor(sum(F7$Pronosticos$Prediccion_Grupo3)))
r8 <- c("Fiscalia 8" , sum(floor(F8$Pronosticos$Prediccion_Grupo1)), floor(sum(F8$Pronosticos$Prediccion_Grupo2)), floor(sum(F8$Pronosticos$Prediccion_Grupo3)))
r9 <- c("Fiscalia 9" , sum(floor(F9$Pronosticos$Prediccion_Grupo1)), floor(sum(F9$Pronosticos$Prediccion_Grupo2)), floor(sum(F9$Pronosticos$Prediccion_Grupo3)))

## Para ficalias sin delitos 
r9 <- c("Fiscalia 9", 0, 0 ,0) 

Demanda <- rbind.data.frame(r1,r2,r3,r4,r5,r6,r7,r8,r9)
Demanda[,2:length(Demanda)] <- as.numeric(unlist(Demanda[,2:length(Demanda)]))
colnames(Demanda) <- c("Fiscalias", "Grupo1" , "Grupo2" , "Grupo3")

## Guardar Datos en Excel 
write.xlsx(Demanda, file = "Datos_Santa_Elena.xlsx", sheetName = "LA LIBERTAD")

