# Importer les données
myData <- read.table(file="data/HR_prediction-train.csv", sep=",", header=T)

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
var_quantitatives <- c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours", "time_spend_company")
var_qualitatives <- c("Work_accident", "promotion_last_5years", "department", "salary")
# Barplot pour les variables qualitatives et sauvegarder les graphiques
for (i in var_qualitatives) {
  barplot(table(myData[,i]), main=i, xlab=i, ylab="Frequence", col="blue")
  dev.copy(png, file=paste("plot_max/",i,".png", sep=""))
  dev.off()
}
