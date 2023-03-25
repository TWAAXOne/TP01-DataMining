### Analyse préliminaire des données
## Importer les données
data <- read.csv("../data/HR_prediction-train.csv")


## Combien y'a t'il d'instance
dim(data) # Connaitre le nombre de lignes et de colonnes

## Pourquoi ? Y a-t-il des données manquantes ?
any(is.na(data)) # Vérifier si il y a des données manquantes -> non




### Analyse exploratoire
## Pour chaque attribut qualitatif f :
## calculer la distribution de probabilité P (f )

# Attributs qualitatifs
qualitative_attributes <- c("Work_accident", "promotion_last_5years", "department", "salary")

# Calcul de la distribution de probabilité pour chaque attribut qualitatif
for (attr in qualitative_attributes) {
  freq_table <- table(data[[attr]])  # table de fréquence -> nombre d'occurrences de la valeur f dans la variable / nombre total d'occurrences
  prob_distribution <- prop.table(freq_table) # Convertir la table de fréquence en distribution de probabilité en divisant chaque valeur par le nombre total d'occurrences
  cat("\nDistribution de probabilité de", attr, ":\n")
  print(prob_distribution)
}

## calculer la probabilité conditionnelle de la variable cible, y,
## compte tenu des valeurs d'attribut P (y|f )
# Variable cible
target_var <- "left"

# Calcul de la probabilité conditionnelle pour chaque attribut qualitatif

# Pour chaque attribut qualitatif
for (attr in qualitative_attributes) {
  # Crée un tableau croisé des fréquences pour chaque combinaison de valeurs de l'attribut qualitatif et de la variable cible
  conditional_table <- table(data[[attr]], data[[target_var]])

  # Convertit le tableau croisé des fréquences en probabilités conditionnelles en divisant chaque fréquence par le nombre d'occurrences de cette valeur de f (en utilisant margin = 1 pour diviser par les totaux des lignes)
  conditional_prob <- prop.table(conditional_table, margin = 1)

  # Affiche la probabilité conditionnelle P(y|f) pour chaque combinaison de valeurs de l'attribut qualitatif et de la variable cible
  cat("\nProbabilite conditionnelle P(", target_var, "|", attr, "):\n")
  print(conditional_prob)
}