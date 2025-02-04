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

# Correction Question 3
compter_nb_elus <- function(df){
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))
  
  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct() |>
    nrow()
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), compter_nb_elus)

# Question 4
library(stringr)
compter_nombre_d_adjoints <- function(df){
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))
  
    sum(str_detect(df$Libellé.de.la.fonction, "adjoint"))
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), compter_nombre_d_adjoints)

# Question 5

validate_schema <- function(df){
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))
}

library(lubridate)
trouver_l_elu_le_plus_age <- function(df){
  validate_schema(df)

  df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance))|>
    slice(which.min(Date.de.naissance))|>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance)
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), trouver_l_elu_le_plus_age)

# Question 6

library(dplyr)
library(lubridate)

calcul_distribution_age <- function(df) {
  validate_schema(df)
  
  df <- df |>  
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>  
    mutate(Age = as.numeric(difftime(Sys.Date(), Date.de.naissance, units = "days")) %/% 365)  # Pas besoin de `!!sym()`
  
  quantiles <- quantile(df$Age, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)
  
  return(quantiles)
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), calcul_distribution_age)

# Question 7

library(dplyr)
library(ggplot2)

plot_code_professions <- function(df) {
  validate_schema(df)
  
  df_counts <- df |> 
    count(Code.de.la.catégorie.socio.professionnelle, name = "Nombre")
  
  bar_chart <- ggplot(df_counts, aes(x = reorder(Code.de.la.catégorie.socio.professionnelle, Nombre), y = Nombre)) +
    geom_bar(stat = "identity", fill = "royalblue") +
    coord_flip() + 
    labs(title = "Nombre d'élus par code professionnel",
         x = "Code professionnel",
         y = "Nombre d'élus") +
    theme_classic()
  print(bar_chart)
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), plot_code_professions)

# Question 8

class(df_Nantes) <- c("commune", class(df_Nantes))
class(df_Faverelles) <- c("commune", class(df_Faverelles))
print(class(df_Nantes))
print(class(df_Faverelles))
  
summary <- function(df) {
  UseMethod("summary")
}

summary.commune <- function(x) {
  print(paste("Libellé de la commune :", unique(x$Libellé.de.la.commune)))
  print(paste("Nombre d'élus dans la commune :", compter_nb_elus(x)))
  print("Distribution de l'âge des élus de la commune :")
  print(calcul_distribution_age(x))
  print("Élu le plus âgé de la commune :")
  print(trouver_l_elu_le_plus_age(x))
}

summary.commune(df_Nantes)
summary.commune(df_Faverelles)
