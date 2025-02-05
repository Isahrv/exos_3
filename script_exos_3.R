# Exercice répertoire national des élus

# library(dplyr) pour distinct()
# library(stringr) pour str_detect()


# Question 2
data_exercice <- read.table("~/cours/M1 ECAP/S2/R avancé & Git/data/elus-conseillers-municipaux-cm.csv", header = TRUE, sep = ";", quote = "")

df_Nantes <- data_exercice[data_exercice$Libellé.de.la.commune == "Nantes",]
df_Faverelles <- data_exercice[data_exercice$Libellé.de.la.commune == "Faverelles",]
df_Loire_Atlantique <- data_exercice[data_exercice$Libellé.du.département == "Loire-Atlantique",]
df_Gers <- data_exercice[data_exercice$Libellé.du.département == "Gers",]

# Question 3
library(dplyr)
compter_nombre_d_elus <- function(df){
  # Cette fonction prend un dataframe correspondant au schéma en entrée et retourne le nombre d'élus par départements ou communes.
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
validate_schema <- function(df){
  # Cette fonction permet d'arrêter le code si jamais le df en entré ne correspond pas au schéma indiqué ci-dessous.
  schema <- c("Code.du.département", "Libellé.du.département", "Code.de.la.collectivité.à.statut.particulier", "Libellé.de.la.collectivité.à.statut.particulier", "Code.de.la.commune", "Libellé.de.la.commune", "Nom.de.l.élu", "Prénom.de.l.élu", "Code.sexe", "Date.de.naissance", "Code.de.la.catégorie.socio.professionnelle", "Libellé.de.la.catégorie.socio.professionnelle", "Date.de.début.du.mandat", "Libellé.de.la.fonction", "Date.de.début.de.la.fonction", "Code.nationalité")
  stopifnot(identical(colnames(df), schema))
}

library(dplyr)
compter_nb_elus <- function(df){
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema() en entrée et retourne le nombre d'élus par départements ou communes.
  validate_schema(df)
  
  df |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Date.de.naissance) |>
    distinct() |>
    nrow()
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), compter_nb_elus)

# Question 4
library(stringr)
compter_nombre_d_adjoints <- function(df){
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema() en entrée et retourne le nombre d'adjoints par départements ou communes.
  validate_schema(df)
  
    sum(str_detect(df$Libellé.de.la.fonction, "adjoint"))
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), compter_nombre_d_adjoints)

# Question 5

library(lubridate)
trouver_l_elu_le_plus_age <- function(df) {
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema() en entrée et retourne le nom, le prénom et l'âge de l'élu le plus agé dans un département/une commune.
  validate_schema(df)
  
  df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
    mutate(Age = as.numeric(difftime(Sys.Date(), Date.de.naissance, units = "days")) %/% 365)|>
  slice(which.max(Age)) |>
  select(Nom.de.l.élu, Prénom.de.l.élu, Age) 
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), trouver_l_elu_le_plus_age)

# Question 6

library(dplyr)
library(lubridate)

calcul_distribution_age <- function(df) {
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema() en entrée et retourne la distribution de l'âge des élus dans un département/une commune.
  validate_schema(df)
  
  df <- df |>  
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>  
    mutate(Age = as.numeric(difftime(Sys.Date(), Date.de.naissance, units = "days")) %/% 365)
  
  quantiles <- quantile(df$Age, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)
  
  return(quantiles)
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), calcul_distribution_age)

# Question 7

library(dplyr)
library(ggplot2)

plot_code_professions <- function(df) {
  # Cette fonction prend un dataframe correspondant au schéma de validate_schema() en entrée et retourne un graphique montrant le nombre d'élus par code professionnel par départements/communes.
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
  # Cette fonction générique prend un dataframe en entrée et a comme objectif de retourner un résumé d'informations différent en fonction de la classe du df.
  UseMethod("summary")
}

summary.commune <- function(x) {
  # Cette fonction prend un dataframe de classe "commune", correspondant au schéma de validate_schema(), en entrée et retourne un résumé d'informations sur une commune.
  print(paste("Libellé de la commune :", unique(x$Libellé.de.la.commune)))
  print(paste("Nombre d'élus dans la commune :", compter_nb_elus(x)))
  print("Distribution de l'âge des élus de la commune :")
  print(calcul_distribution_age(x))
  print("Élu le/la plus âgé.e de la commune :")
  print(trouver_l_elu_le_plus_age(x))
}

summary.commune(df_Nantes)
summary.commune(df_Faverelles)

# Question 9

class(df_Loire_Atlantique) <- c("departement", class(df_Loire_Atlantique))
class(df_Gers) <- c("departement", class(df_Gers))
print(class(df_Loire_Atlantique))
print(class(df_Gers))

compter_nb_commune <- function(df){
  # Cette fonction prend un dataframe sur un département, correspondant au schéma de validate_schema(), en entrée et retourne le nombre de commune dans un département.
  validate_schema(df)
  
  df |>
    select(Libellé.de.la.commune) |>
    distinct() |>
    nrow()
}

sapply(list(df_Loire_Atlantique, df_Gers), compter_nb_commune)

trouver_l_elu_le_plus_jeune <- function(df) {
  # Cette fonction prend un dataframe  correspondant au schéma de validate_schema() en entrée et retourne le nom, le prénom et l'âge de l'élu le plus jeune dans un département/une commune.
  validate_schema(df)
  
  df |>
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
    mutate(Age = as.numeric(difftime(Sys.Date(), Date.de.naissance, units = "days")) %/% 365)|>
    slice(which.min(Age)) |>
    select(Nom.de.l.élu, Prénom.de.l.élu, Age) 
}

sapply(list(df_Nantes, df_Faverelles, df_Loire_Atlantique, df_Gers), trouver_l_elu_le_plus_jeune)


summary.departement <- function(x) {
  # Cette fonction prend un dataframe de classe "departement", correspondant au schéma de validate_schema(), en entrée et retourne un résumé d'informations sur un département.
  print(paste("Nom du département :", unique(x$Libellé.du.département)))
  print(paste("Nombre de commune :", compter_nb_commune(x)))
  print(paste("Nombre d'élus dans le département :", compter_nb_elus(x)))
  print("Distribution de l'âge des élus du département :")
  print(calcul_distribution_age(x))
  
  print("Élu(e) le/la plus âgé(e) du département :")
  elu_plus_age <- trouver_l_elu_le_plus_age(x)
  print(elu_plus_age)
  print(paste("Commune de l'élu(e) le/la plus âgé(e) :", x$Libellé.de.la.commune[x$Nom.de.l.élu == elu_plus_age$Nom.de.l.élu & x$Prénom.de.l.élu == elu_plus_age$Prénom.de.l.élu]))
  
  print("Élu(e) le/la plus jeune du département :")
  elu_plus_jeune <- trouver_l_elu_le_plus_jeune(x)
  print(elu_plus_jeune)
  print(paste("Commune de l'élu(e) le/la plus jeune :", x$Libellé.de.la.commune[x$Nom.de.l.élu == elu_plus_jeune$Nom.de.l.élu & x$Prénom.de.l.élu == elu_plus_jeune$Prénom.de.l.élu]))
  
  age <- x |> 
    mutate(Date.de.naissance = dmy(Date.de.naissance)) |>
    mutate(Age = as.numeric(difftime(Sys.Date(), Date.de.naissance, units = "days")) %/% 365)

  moyenne_age_par_commune <- age |>
    group_by(Libellé.de.la.commune) |>
    summarise(Moyenne_Age = mean(Age, na.rm = TRUE), .groups = "drop")

  commune_age_min <- moyenne_age_par_commune |>
    slice(which.min(Moyenne_Age)) |>
    pull(Libellé.de.la.commune)

  commune_age_max <- moyenne_age_par_commune |>
    slice(which.max(Moyenne_Age)) |>
    pull(Libellé.de.la.commune)
  
  print(paste("Commune avec la moyenne d'âge la plus faible :", commune_age_min))
  print("Distribution des âges pour cette commune :")
  print(calcul_distribution_age(filter(x, Libellé.de.la.commune == commune_age_min)))
  
  print(paste("Commune avec la moyenne d'âge la plus élevée :", commune_age_max))
  print("Distribution des âges pour cette commune :")
  print(calcul_distribution_age(filter(x, Libellé.de.la.commune == commune_age_max)))
}

summary.departement(df_Loire_Atlantique)
summary.departement(df_Gers)

# Question 10

library(dplyr)
library(ggplot2)

plot <- function(df) {
  # Cette fonction générique prend un dataframe en entrée et a comme objectif de retourner un graphique différent en fonction de la classe du df.
  UseMethod("plot")
}

plot.commune <- function(df) {
  # Cette fonction prend un dataframe de classe "commune", correspondant au schéma de validate_schema(), en entrée et retourne un graphique montrant la distribution d'élus par rapport aux codes professionnels.
  validate_schema(df)
  
  df_counts <- df |> 
    count(Code.de.la.catégorie.socio.professionnelle, name = "Nombre")
  
  nom_commune <- unique(df$Libellé.de.la.commune)
  nom_departement <- unique(df$Libellé.du.département)
  nb_elus <- sum(df_counts$Nombre)
  
  titre_graphique <- paste(nom_commune, "-", nom_departement)
  axe_x <- paste("Libellés des codes professionnels pour les", nb_elus, "élus")
  
  bar_chart <- ggplot(df_counts, aes(x = reorder(Code.de.la.catégorie.socio.professionnelle, Nombre), y = Nombre)) +
    geom_bar(stat = "identity", fill = "royalblue") +
    coord_flip() + 
    labs(title = titre_graphique,
         x = axe_x,
         y = "Nombre d'élus") +
    theme_classic()
  
  print(bar_chart)
}

sapply(list(df_Nantes, df_Faverelles), plot.commune)

# Question 11

plot.departement <- function(df) {
  # Cette fonction prend un dataframe de classe "departement", correspondant au schéma de validate_schema(), en entrée et retourne un graphique montrant la distribution d'élus par rapport aux 10 codes professionnels les plus représentés dans le département.
  validate_schema(df)
  
  df_counts <- df |> 
    count(Code.de.la.catégorie.socio.professionnelle, name = "Nombre") |> 
    arrange(desc(Nombre)) |> 
    slice_head(n = 10)
  
  nom_departement <- unique(df$Libellé.du.département)
  nb_communes <- length(unique(df$Libellé.de.la.commune))
  
  titre_graphique <- paste(nom_departement, "-", nb_communes, "communes")
  axe_x_label <- paste("Libellés des 10 codes professionnels les plus représentés pour", nom_departement)
  
  bar_chart <- ggplot(df_counts, aes(x = reorder(Code.de.la.catégorie.socio.professionnelle, Nombre), y = Nombre)) +
    geom_bar(stat = "identity", fill = "royalblue") +
    coord_flip() + 
    labs(title = titre_graphique,
         x = axe_x_label,
         y = "Nombre d'élus") +
    theme_classic()
  
  print(bar_chart)
}

sapply(list(df_Loire_Atlantique, df_Gers), plot.departement)
