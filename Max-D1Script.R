runAnalysis <- function(datasetName, DiscreteAttributesIndeces, AttrsToRemoveIndeces, ClassAttributeIndex) {
  
  # Chargez les données
  data <- read.csv(datasetName)
  
  # Supprimez les attributs inutiles
  data <- data[, -AttrsToRemoveIndeces]  # prendre toutes les lignes et toutes les colonnes sauf celles à supprimer

  # Attributs discrets (qualitatifs) et continus (quantitatifs)
  discrete_attributes <- data[, setdiff(DiscreteAttributesIndeces, ClassAttributeIndex)] # setdiff: différence de deux ensembles (prends toutes les valeurs de DiscreteAttributesIndeces sauf celles de ClassAttributeIndex)
  continuous_attributes <- data[, -c(DiscreteAttributesIndeces, ClassAttributeIndex)]

  cat("Pourquoi séparer les attributs discrets et continus ?
  Il est important de séparer les attributs discrets et continus,
  car l'analyse exploratoire et les visualisations appropriées pour ces types de variables sont différentes.
  Par exemple, pour les attributs continus, on peut utiliser des histogrammes,
  des boîtes à moustaches ou des nuages de points pour visualiser la distribution des données.
  Pour les attributs discrets, on utilise généralement des diagrammes en barres ou des tableaux de fréquence.
  ")

  # Analyse des attributs continus (quantitatifs)
  for (i in colnames(continuous_attributes)) {
    hist(continuous_attributes[[i]], main=i, xlab=i)
  }

    # Analyse des attributs discrets (qualitatifs)
    for (i in colnames(discrete_attributes)) {
      barplot(table(discrete_attributes[[i]]), main=i, xlab=i)
    }
}

runAnalysis("data/HR_prediction-train.csv", c(6, 7, 9, 10), 1, 8)