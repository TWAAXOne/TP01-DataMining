### Analyse préliminaire des données
## Importer les données
data <- read.csv("./data/HR_prediction-train.csv", sep = ",", header = TRUE)


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

### Pour chaque attribut quantitatif f :
## Calculer la moyenne μ(f ) et la variance σ2 (f )
# Attributs quantitatifs
quantitative_attributes <- c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours", "time_spend_company")
# Calcul de la moyenne et de la variance pour chaque attribut quantitatif
for (attr in quantitative_attributes) {
  mean <- mean(data[[attr]])
  variance <- var(data[[attr]])
  cat("\nMoyenne de", attr, ":", mean)
  cat("\nVariance de", attr, ":", variance)
}

## Calculer la moyenne u(f | target) et la variance σ2 (f | target) conditionnelles à la variable cible
# Calcul de la moyenne et de la variance conditionnelles pour chaque attribut quantitatif

quitter <- data[data[[target_var]] == 1,] # extrait les lignes de l'ensemble de données 'data' pour lesquelles la condition précédente est vraie
pas_quitter <- data[data[[target_var]] == 0,]

for (attr in quantitative_attributes) {
  mean_quitter <- mean(quitter[[attr]])
  mean_pas_quitter <- mean(pas_quitter[[attr]])
  variance_quitter <- var(quitter[[attr]])
  variance_pas_quitter <- var(pas_quitter[[attr]])

  # Afficher les résultats
  cat("\n",attr, ":\n")
  cat("Moyenne conditionnelle de ", attr, " pour les employes qui ont quitte l'entreprise :", mean_quitter, "\n")
  cat("Moyenne conditionnelle de ", attr, " pour les employes qui sont restes :", mean_pas_quitter, "\n")
  cat("Variance conditionnelle de ", attr, " pour les employes qui ont quitte l'entreprise :", variance_quitter, "\n")
  cat("Variance conditionnelle de ", attr, " pour les employes qui sont restes :", variance_pas_quitter, "\n")
}

## Classez les variables en fonction de leur importance en calculant la différence
## de moyenne conditonnelle de la classe mise à l'échelle par l'écart-type
# Crée un vecteur pour stocker les résultats
importance <- c()

# Boucles sur les attributs quantitatifs
for (attr in quantitative_attributes){

  # Calcule la moyenne conditionnelle de la classe pour l'attribut
  mean_left <- mean(quitter[[attr]])
  mean_not_left <- mean(pas_quitter[[attr]])

  # Calcule la différence de moyenne conditionnelle
  mean_diff <- abs(mean_left - mean_not_left) # abs = valeur absolue, comme ca c'est toujours positif

  # Calculer l'écart-type pour chaque classe de variable cible
  sd_left <- sd(quitter[[attr]])
  sd_not_left <- sd(pas_quitter[[attr]])

  # Calculer la moyenne de l'ecart type
  mean_sd <- mean(c(sd_left, sd_not_left))

  # Calcule la différence de moyenne conditionnelle mise à l'échelle par l'écart-type
  scaled_diff <- mean_diff / mean_sd

  # Ajoute le résultat au vecteur d'importance
  importance <- c(importance, scaled_diff)
}

# Crée un dataframe avec les noms des attributs quantitatifs et leur importance
importance_df <- data.frame(Attribute = quantitative_attributes, Importance = importance)

# Trie les attributs par importance décroissante
importance_ranked <- importance_df[order(-importance_df$Importance), ]

# Affiche le classement des attributs par importance
print(importance_ranked)


### Partie 5
### Choisissez une variable quantitative, f, et
## écrivez la distribution normale pour les cas suivants : P (f ),
## P (f |y), expliquez ce qui change entre les différentes distributions.
mean_f <- mean(data$satisfaction_level, na.rm = TRUE)
sd_f <- sd(data$satisfaction_level, na.rm = TRUE)

mean_f_left <- mean(quitter$satisfaction_level, na.rm = TRUE)
sd_f_left <- sd(quitter$satisfaction_level, na.rm = TRUE)

mean_f_not_left <- mean(pas_quitter$satisfaction_level, na.rm = TRUE)
sd_f_not_left <- sd(pas_quitter$satisfaction_level, na.rm = TRUE)


# P(f)
hist(data$satisfaction_level, main = "Histogramme de satisfaction_level",
     xlab = "satisfaction_level", col = "lightblue", border = "black", freq = FALSE)

curve(dnorm(x, mean = mean_f, sd = sd_f), add = TRUE, col = "red", lwd = 2)

# P(f|y)
hist(quitter$satisfaction_level, main = "Histogramme de satisfaction_level | left",
     xlab = "satisfaction_level", col = "lightblue", border = "black", freq = FALSE)

curve(dnorm(x, mean = mean_f_left, sd = sd_f_left), add = TRUE, col = "red", lwd = 2)

hist(pas_quitter$satisfaction_level, main = "Histogramme de satisfaction_level | not_left",
     xlab = "satisfaction_level", col = "lightblue", border = "black", freq = FALSE)

curve(dnorm(x, mean = mean_f_not_left, sd = sd_f_not_left), add = TRUE, col = "red", lwd = 2)

### Calculer la probabilité à priori P(y) et la probabilité à posteriori P(y|f )
prop_f <- length(data$satisfaction_level) / length(data$satisfaction_level)
prop_f_left <- length(quitter$satisfaction_level) / length(data$satisfaction_level)
prop_f_not_left <- length(pas_quitter$satisfaction_level) / length(data$satisfaction_level)

cat("Proportion P(satisfaction_level) :", prop_f, "\n")
cat("Proportion P(satisfaction_level|left) :", prop_f_left, "\n")
cat("Proportion P(satisfaction_level|not_left) :", prop_f_not_left, "\n")


### Partie 6
for (attr in qualitative_attributes) {
  # Probability distribution
  barplot(table(data[,attr])/nrow(data), main=paste("Probability distribution of", attr), xlab=attr, ylab="Probability")
  # Conditional probabilities
  barplot(table(data[,attr], data[,target_var])/nrow(data), main=paste("Conditional probabilities of", target_var, "given", attr), xlab=attr, ylab="Probability")
}

# For each quantitative attribute, visualize the distribution and the conditional distributions by histograms
for (attr in quantitative_attributes) {
  # Distribution
  hist(data[,attr], freq=FALSE, breaks=20, col="blue", main=paste("Histogram of", attr), xlab=attr)
  # Conditional distributions
  hist(data[data[,target_var]==1,attr], freq=FALSE, breaks=20, col="blue", main=paste("Histogram of", attr, "given", target_var), xlab=attr)
}

# Sélectionner les deux attributs les plus importants
# satisfaction_level et last_evaluation
plot(data[,c("satisfaction_level", "last_evaluation")], col=data[,target_var]+1, main="Scatter plot of satisfaction_level and last_evaluation", xlab="satisfaction_level", ylab="last_evaluation")
