dataSet <- read.table(file = "data/HR_prediction-train.csv", header = TRUE)
# number of instances
n <- nrow(dataSet)
#
dim(dataSet)
# quel est la variable cible ?
# la variable cible est la variable "left"
#montre moi les valeurs manquantes
table(is.na(dataSet))
# il n'y a pas de valeurs manquantes
#Pour chaque attributs qualitatif f , calcule la distribution de probabilité de la variable cible



