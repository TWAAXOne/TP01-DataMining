# you might you to move to the directory that contains you data
setwd("/Users/daniel_avelino/Documents/_HEG/6eme_semestre/62-62_data_mining/TP1/TP1/dataset")

# read the data and store them in myData
myData <- read.table(file="HR_prediction-train.csv", sep=",", header=T)

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

# Spécifiez les noms des attributs qualitatifs dans un vecteur
qualitative_attrs <- c("Work_accident", "left", "promotion_last_5years", "department", "salary")

# Boucle pour chaque attribut qualitatif
for (i in qualitative_attrs) {
  barplot(table(myData[,i]), main=i, xlab=i, ylab="Frequence", col="blue")
  quartz()
  # dev.copy(png, file=paste0("plot_daniel/", i, ".png"))
  # dev.off()
}

# Spécifiez les noms des attributs quantitatifs dans un vecteur
quantitative_attrs <- c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours", "time_spend_company")

# Boucle pour chaque attribut quantitatif
for (i in quantitative_attrs) {
  hist(myData[,i], main=i, xlab=i, ylab="Frequence", col="red")
  # dev.copy(png, file= paste0("plot_max/hist-", i, ".png"))
  quartz()
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
  print(paste("Probabilité marginale de l'attribut", f, ":\n"))
  print(prob_f)
  print(paste("Probabilité conditionnelle de la variable cible 'left' compte tenu des valeurs d'attribut", f, ":\n"))
  print(prob_left_given_f)
}

# calculer la probabilité conditionnelle de la variable cible, y, compte tenu des valeurs d'attribut P (y|f )
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

# bayes
# Spécifiez l'attribut qualitatif f et la variable cible y
f <- "department"
y <- "left"

# Calculez la probabilité marginale de f et de y
prob_f_bayes <- table(myData[[f]])/nrow(myData)
prob_y_bayes <- table(myData[[y]])/nrow(myData)

# Calculez la probabilité conjointe de f et y
prob_f_and_y_bayes <- table(myData[[f]], myData[[y]])/nrow(myData)

# Calculez la probabilité conditionnelle de y sachant f avec la règle de Bayes
prob_y_given_f_bayes <- NULL
for(i in 1:length(prob_f_bayes)) {
  if(prob_f[i] > 0) {
    prob_y_given_f_bayes[i] <- prob_f_and_y_bayes[i,]*prob_y_bayes/prob_f_bayes[i]
  }
}

# Afficher les résultats
print(paste("Probabilité conditionnelle de", y, "sachant", f, "avec la règle de Bayes:\n"))
print(prob_y_given_f_bayes)




