library(writexl)
library(readxl)
library(stringr)
#### Concatenar tablas de Resumen.
P <- names(table(DatxProv$Provincias))
P[3] = "CANAR"
Tott <- data.frame()
for (p in P) {
  inf <- read_excel(str_c("Resultados_", p, ".xlsx"), sheet = "Resumen")
  inf$Provincia <- rep(p,dim(inf)[1])
  print(inf)
  Tott <- rbind(Tott, inf)
}

write_xlsx(Tott, path = "Resumen_Total.xlsx")



#### Tablas de Asignación.
Tott <- read_excel(str_c("Resultados_AZUAY.xlsx"), sheet = "Fiscalias")
Tott <- rbind(rep("AZUAY",dim(Tott)[2]),names(Tott), Tott)
P <- names(table(DatxProv$Provincias))
P <- P[2:24]
length(P)
for (p in P) {
  inf <- read_excel(str_c("Resultados_", p, ".xlsx"), sheet = "Fiscalias")
  Tott <- cbind(Tott, rbind(rep(p,dim(inf[2:dim(inf)[2]])[2]),names(inf[2:dim(inf)[2]]), inf[2:dim(inf)[2]]))
}

Tott[Tott$Cantones == "ANTONIO ELIZALDE",]$Cantones <- "GNRAL. ANTONIO ELIZALDE" 
Tott[Tott$Cantones == "MARCELINO MARIDUENA",]$Cantones <- "CRNEL. 	MARCELINO MARIDUENA" 

df_transpose <- data.frame(t(Tott[-1]))
# Añadimos los nombres de las columnas
colnames(df_transpose) <- Tott[, 1]
df_transpose

write_xlsx(df_transpose, path = " Asignacion_Totales.xlsx")
