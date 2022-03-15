### Graficos de Calor.

setwd("~/DOC_TESIS_FINAL/CÓDIGOS R") ## ruta de documentos
source("Lectura_Datos.R") ## 2 minutos en leer archivos 
source("funciones1.R") ## Funciones para resultados

# 1.1 Resultados a nivel nacional
library(kableExtra) ## Libreria para mostrar tablas

DatxProv <- Frec_Prov(DXSiaf)
data_ecuador <- Grafico_D(DatxProv) ## Datos para graficos de ECUADOR
ecuador <- rjson::fromJSON( file = "https://raw.githubusercontent.com/Rusersgroup/mapa_ecuador/master/ec-all.geo.json") ## DATOS DE ECUADOR
highchart() %>%
  hc_title(text = "<i>Mapa dinámico de Ecuador</i>",
           margin = 20, align = "center",
           style = list(color = "#08338F", useHTML = TRUE)) %>% 
  hc_subtitle(text = "Deficit de fiscales por provincia",
              align = "center",
              style = list(color = "#0C5C9E", fontWeight = "bold")) %>% 
  hc_tooltip(followPointer =  FALSE) %>%
  hc_add_series_map(ecuador, data_ecuador, name = "Prosecutors",
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


Provincia <- "PICHINCHA"
PROVINCIA <- Datos_de_p(DXSiaf,Provincia) # Elijo data de la Provincia en estudio.
## Datos para el mapa de la provincia.
Frec <- Grafico_P(Provincia,PROVINCIA) ## Datos de frecuecnia en orde de la data para mapa
library(sf)
cantones <- st_read("ecuador.json")
PROVINCIA1 <- cantones[cantones$DPA_DESPRO == chartr("Ñ", "Ð", toupper(Provincia)) ,]

ggplot(data = PROVINCIA1) +
  geom_sf(aes(fill = Frec$CANTIDAD ))+ labs(title = "",
                                            fill = "Delitos")+ geom_text(data = Frec$COORDE, aes(X, Y, label = canton), colour = "black", size=3) + scale_fill_gradient(low="yellow", high="red") 
