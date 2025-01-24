# Exercice répertoire national des élus

# Question 2
data_exercice <- read.table("~/cours/M1 ECAP/S2/R avancé & Git/data/elus-conseillers-municipaux-cm.csv", header = TRUE, sep = ";", quote = "")

df_Nantes <- data_exercice[data_exercice$Libellé.de.la.commune == "Nantes",]
df_Faverelles <- data_exercice[data_exercice$Libellé.de.la.commune == "Faverelles",]
df_Loire_Atlantique <- data_exercice[data_exercice$Libellé.du.département == "Loire-Atlantique",]
df_Gers <- data_exercice[data_exercice$Libellé.du.département == "Gers",]
