source("funciones2.R")
library(kableExtra)

#Resultados de la Tasa

kbl(Tasa()$Flagrantes) %>% kable_styling()
kbl(Tasa()$No_Flagrantes) %>% kable_styling()
kbl(Tasa()$Archivados) %>% kable_styling()

#Resultados Demandas por provincia.

Provincia <- "SANTA ELENA"

ResProv(Provincia)



