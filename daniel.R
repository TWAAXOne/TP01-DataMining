# read the data and store them in myData
myData <- read.table(file="data/HR_prediction-train.csv", sep=",", header=T)

# count nb of instances
# nrow(myData)

# attributs quantitatifs
# dep <- table(myData$department)/nrow(myData)
# sal <- table(myData$salary)/nrow(myData)

# attributs qualitatifs
# qual <- table(dataset$department)/nrow(dataset)

# dep
# sal

# Afficher les noms de colonnes dans le dataset
names(myData)

# Analyse exploratoire
# calculer la distribution de probabilité P (f)
# Spécifiez les noms des attributs qualitatifs dans un vecteur
qualitative_attrs <- c("Work_accident", "left", "promotion_last_5years", "department", "salary")

# Boucle pour chaque attribut qualitatif
# for (i in qualitative_attrs) {
#   barplot(table(myData[,i]), main=i, xlab=i, ylab="Frequence", col="blue")
#   dev.copy(png, file= paste0("plot_daniel/bar-", i, ".png"))
#   dev.off()
# }

# Spécifiez les noms des attributs quantitatifs dans un vecteur
quantitative_attrs <- c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours", "time_spend_company")

# Boucle pour chaque attribut quantitatif
# for (i in quantitative_attrs) {
#   hist(myData[,i], main=i, xlab=i, ylab="Frequence", col="blue")
#   dev.copy(png, file= paste0("plot_daniel/hist-", i, ".png"))
#   dev.off()
# }
#------------------------------------------------------------------------

# Partie 2
# calculer la probabilité conditionnelle de la variable cible, y, compte tenu des valeurs d'attribut P (y|f)
# Spécifiez les noms des attributs f pour lesquels vous voulez calculer la probabilité conditionnelle
f_attrs <- c("department", "salary")

# Boucle pour chaque attribut f
for(f in f_attrs) {
  # Calculez la probabilité marginale de l'attribut f
  prob_f <- table(myData[[f]])/nrow(myData)

  # Calculez la probabilité conditionnelle de la variable cible "left" compte tenu des valeurs d'attribut f
  prob_left_given_f <- tapply(myData$left, myData[[f]], function(x) table(x)/length(x))

  # Affichez les résultats
  print(paste("Probabilité marginale de l'attribut", f, ":\n"))
  print(prob_f)
  print(paste("Probabilité conditionnelle de la variable cible 'left' compte tenu des valeurs d'attribut", f, ":\n"))
  print(prob_left_given_f)
}

# Partie 3
# Spécifiez l'attribut qualitatif f et la variable cible y
f <- "department"
y <- "left"

# Calculez la probabilité marginale de f et de y
prob_f <- table(myData[[f]])/nrow(myData)
prob_y <- table(myData[[y]])/nrow(myData)

# Calculez la probabilité conjointe de f et y
prob_f_and_y <- table(myData[[f]], myData[[y]])/nrow(myData)

# Calculez la probabilité conditionnelle de f sachant y
prob_f_given_y <- NULL
for(i in 1:length(prob_y)) {
  if(prob_y[i] > 0) {
    prob_f_given_y <- prob_f_and_y[,i]/prob_y[i]
  }
}

# Calculez la probabilité conditionnelle de y sachant f
prob_y_given_f <- NULL
for(i in 1:length(prob_f)) {
  if(prob_f[i] > 0) {
    prob_y_given_f <- prob_f_and_y[i,]/prob_f[i]
  }
}

# Afficher les résultats
print(paste("Probabilité conditionnelle de", f, "sachant", y, ":\n"))
print(prob_f_given_y)
print(paste("Probabilité conditionnelle de", y, "sachant", f, ":\n"))
print(prob_y_given_f)

# Spécifiez l'attribut qualitatif f et la variable cible y
f <- "department"
y <- "left"

# Calculez la probabilité marginale de f et de y
prob_f <- table(myData[[f]])/nrow(myData)
prob_y <- table(myData[[y]])/nrow(myData)

# Calculez la probabilité conjointe de f et y
prob_f_and_y <- table(myData[[f]], myData[[y]])/nrow(myData)

# Calculez la probabilité conditionnelle de f sachant y
prob_f_given_y <- matrix(0, nrow=length(prob_f), ncol=length(prob_y))
for(i in 1:length(prob_y)) {
  if(prob_y[i] > 0) {
    prob_f_given_y[,i] <- prob_f_and_y[,i]/prob_y[i]
  }
}

# Calculez la probabilité conditionnelle de y sachant f avec la règle de Bayes
prob_y_given_f <- matrix(0, nrow=length(prob_f), ncol=length(prob_y))
for(i in 1:length(prob_f)) {
  if(prob_f[i] > 0) {
    prob_y_given_f[i,] <- prob_f_given_y[i,]*prob_y/prob_f[i]
  }
}

# Afficher les résultats
print(paste("Probabilité conditionnelle de", y, "sachant", f, "avec la règle de Bayes:\n"))
print(prob_y_given_f)
# ------------------------------------------------------------------------

# Partie 4
# Sélectionner les colonnes quantitatives
dataset_quantitatif <- myData[, c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours", "time_spend_company")]

# Calculer la moyenne et la variance pour chaque attribut quantitatif
moyennes <- colMeans(dataset_quantitatif)
variances <- sapply(dataset_quantitatif, var)

# Afficher les moyennes et les variances
print(moyennes)
print(variances)

# Calculer la moyenne et la variance conditionnelles pour chaque attribut quantitatif et pour chaque classe de la variable cible
dataset_0 <- dataset_quantitatif[myData$left == 0, ]
dataset_1 <- dataset_quantitatif[myData$left == 1, ]

moyennes_0 <- colMeans(dataset_0)
moyennes_1 <- colMeans(dataset_1)

variances_0 <- sapply(dataset_0, var)
variances_1 <- sapply(dataset_1, var)

# Afficher les moyennes et variances conditionnelles
print(moyennes_0)
print(moyennes_1)
print(variances_0)
print(variances_1)

# Calculer les scores d'importance
importance_scores <- data.frame(
  Variable = colnames(dataset_quantitatif),
  Importance = abs(moyennes_0 - moyennes_1) / sqrt(variances_0 + variances_1)
)

# Trier les variables par ordre d'importance
sorted_importance_scores <- importance_scores[order(-importance_scores$Importance), ]

print("Scores d'importance des variables quantitatives :")
print(sorted_importance_scores)
# ------------------------------------------------------------------------

# Partie 5
# Je vais choisir la variable quantitative "satisfaction_level" pour cette démonstration.
#
# Distribution normale pour P(f) :
# La distribution normale pour P(f) est définie par la moyenne et la variance de la variable "satisfaction_level" calculées précédemment :
#
# P(f) ~ N(µ(f), σ²(f))
#
# où µ(f) est la moyenne de "satisfaction_level" et σ²(f) est la variance de "satisfaction_level".
#
# Distribution normale pour P(f|y) :
# La distribution normale pour P(f|y) est définie par la moyenne et la variance conditionnelles de la variable "satisfaction_level" pour chaque classe de la variable cible 'left' :
#
# P(f|y = 0) ~ N(µ(f|y = 0), σ²(f|y = 0))
# P(f|y = 1) ~ N(µ(f|y = 1), σ²(f|y = 1))
#
# où µ(f|y = i) est la moyenne conditionnelle de "satisfaction_level" pour la classe i et σ²(f|y = i) est la variance conditionnelle de "satisfaction_level" pour la classe i.
#
# Ce qui change entre les différentes distributions est la moyenne et la variance, qui sont conditionnées par la variable cible 'left' pour P(f|y).

# Ajuster les marges de la figure
par(mar = c(5, 4, 4, 2) + 0.1)

# Créer un histogramme pour P(f)
hist(myData$satisfaction_level, freq = FALSE, col = "lightblue", main = "Histogramme de la variable 'satisfaction_level'", xlab = "satisfaction_level")
# Ajouter une courbe de distribution normale pour P(f)
curve(dnorm(x, mean = moyennes["satisfaction_level"], sd = sqrt(variances["satisfaction_level"])), col = "blue", lwd = 2, add = TRUE)

# Créer un histogramme pour P(f|y = 0)
par(mar = c(5, 4, 4, 2) + 0.1)
hist(dataset_0$satisfaction_level, freq = FALSE, col = "lightblue", main = "Histogramme de 'satisfaction_level' pour 'left' = 0", xlab = "satisfaction_level")
# Ajouter une courbe de distribution normale pour P(f|y = 0)
curve(dnorm(x, mean = moyennes_0["satisfaction_level"], sd = sqrt(variances_0["satisfaction_level"])), col = "blue", lwd = 2, add = TRUE)

# Créer un histogramme pour P(f|y = 1)
par(mar = c(5, 4, 4, 2) + 0.1)
hist(dataset_1$satisfaction_level, freq = FALSE, col = "lightblue", main = "Histogramme de 'satisfaction_level' pour 'left' = 1", xlab = "satisfaction_level")
# Ajouter une courbe de distribution normale pour P(f|y = 1)
curve(dnorm(x, mean = moyennes_1["satisfaction_level"], sd = sqrt(variances_1["satisfaction_level"])), col = "blue", lwd = 2, add = TRUE)
# ------------------------------------------------------------------------