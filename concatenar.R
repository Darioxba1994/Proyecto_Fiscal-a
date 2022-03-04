library(writexl)
Tott <- data.frame()
for (p in P) {
  inf <- read_excel(str_c("Resultados_", p, ".xlsx"), sheet = "Resumen")
  inf$Provincia <- rep(p,dim(inf)[1])
  print(inf)
  Tott <- rbind(Tott, inf)
}
Tott[Tott$Cantones == "ANTONIO ELIZALDE",]$Cantones <- "GNRAL. ANTONIO ELIZALDE" 
Tott[Tott$Cantones == "MARCELINO MARIDUENA",]$Cantones <- "CRNEL. 	MARCELINO MARIDUENA" 
Tott <- arrange(Tott, Cantones )