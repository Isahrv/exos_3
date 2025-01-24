# Exercice répertoire national des élus

# Question 2
data_exercice <- read.table("~/cours/M1 ECAP/S2/R avancé & Git/data/elus-conseillers-municipaux-cm.csv", header = TRUE, sep = ";", quote = "")

df_Nantes <- data_exercice[data_exercice$Libellé.de.la.commune == "Nantes",]
df_Faverelles <- data_exercice[data_exercice$Libellé.de.la.commune == "Faverelles",]
df_Loire_Atlantique <- data_exercice[data_exercice$Libellé.du.département == "Loire-Atlantique",]
df_Gers <- data_exercice[data_exercice$Libellé.du.département == "Gers",]

# Question 3
library(dplyr)
compter_nombre_d_elus <- function(df){
  unique_elus <- df |>
    count(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)
  nombre_elus <- sum(unique_elus$n)
  return(list(unique_elus = unique_elus, nombre_elus = nombre_elus))
}

compter_nombre_d_elus(df_Nantes)
compter_nombre_d_elus(df_Faverelles)
compter_nombre_d_elus(df_Loire_Atlantique)
compter_nombre_d_elus(df_Gers)

