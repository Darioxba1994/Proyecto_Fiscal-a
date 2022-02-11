source("FUNC_FINAL.R")

#Lista de Provincias 
Provincias <- names(table(DXSiaf$Provincia))
## Predicciones de todas las provincias.
library(dplyr)
for (p in Provincias) {
    ResProv(p)
}

p <- "PICHINCHA"
a <- ResProv(p)
View(a)
library(xlsx)
write.xlsx(a, file = "Parametros_Provincia.xlsx", sheetName = "Todos")


