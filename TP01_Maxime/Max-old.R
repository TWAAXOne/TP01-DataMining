# Importer les données
myData <- read.table(file= "../data/HR_prediction-train.csv", sep=",", header=T)

################# Anaylser préliminaire
# Combien y'a t'il d'instance
nrow(myData)
  # or
myData[0]
# Combien y'a t'il de variables
ncol(myData)

# Savoir si il y a des données manquantes
any(is.na(myData))


################# Analyse exploratoire
# calculer la distribution de probabilité P (f ) pour chaque variable
# P (f ) = nombre d’occurrences de la valeur f dans la variable / nombre total d’occurrences
var_qualitatives <- c("Work_accident", "promotion_last_5years", "department", "salary")

# Barplot pour les variables qualitatives et sauvegarder les graphiques
for (i in var_qualitatives) {
  barplot(table(myData[,i]), main=i, xlab=i, ylab="Frequence", col="blue")
  dev.copy(png, file= paste0("plot_max/bar-", i, ".png"))
  dev.off() # Ajouter cette ligne pour fermer correctement le fichier png
}

var_quantitatives <- c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours", "time_spend_company")
# Histogramme pour les variables quantitatives et sauvegarder les graphiques
for (i in var_quantitatives) {
  hist(myData[,i], main=i, xlab=i, ylab="Frequence", col="blue")
  dev.copy(png, file= paste0("plot_max/hist-", i, ".png"))
  dev.off() # Ajouter cette ligne pour fermer correctement le fichier png
}

# Spécifiez les noms des attributs f pour lesquels vous voulez calculer la probabilité conditionnelle
f_attrs <- c("department", "salary")

# Boucle pour chaque attribut f
for(f in f_attrs) {
  # Calculez la probabilité marginale de l'attribut f
  prob_f <- table(myData[[f]])/nrow(myData)

  # Calculez la probabilité conditionnelle de la variable cible "left" compte tenu des valeurs d'attribut f
  prob_left_given_f <- tapply(myData$left, myData[[f]], function(x) table(x)/length(x))

  # Affichez les résultats
  print(paste("Probabilité marginale de l'attribut", f, ":\\n"))
  print(prob_f)
  print(paste("Probabilité conditionnelle de la variable cible 'left' compte tenu des valeurs d'attribut", f, ":\\n"))
  print(prob_left_given_f)
}

