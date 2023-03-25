# -------------------------------------
# 1. Premilinary analysis
# -------------------------------------

# get the current working directory
getwd()

# read csv file
myData <- read.table("C:/Users/elian/Documents/HEG/3eme annee/Semestre 6/DataMining/Data Mining/untitled/TPS/HR_prediction-train.csv", sep=",", header=1)

# get the number of instances and variables
dim(myData)

# Check if there is any missing data
sum(is.na(myData)) # returns the number of missing values
any(is.na(myData)) # returns TRUE if there is at least one missing value


# -------------------------------------
# 2. Exploration Analysis
# -------------------------------------
print("--------------------------------------------------")
# Qualitative attributes
qualitative_attrs <- c("Work_accident", "left", "promotion_last_5years", "department", "salary")
# Compute the probability distribution of each qualitative attribute
for (attr in qualitative_attrs) {
  print(attr)
  print(table(myData[,attr]))
}

print("--------------------------------------------------")
# The target value is "left" in the qualitative attributes, compute the conditional probability distribution of "left" given each qualitative attribute
for (attr in qualitative_attrs) {
  print(attr)
  print(table(myData[,attr], myData[,"left"]))
}

# -------------------------------------
# 3. Part 3
# -------------------------------------
# Specifying the target variable and a qualitative attribute
target <- "left"
qualitative_attr <- "department"

# Calculate the marginal probability of target and qualitative_attr
target_prob <- table(myData[,target])/nrow(myData)
qualitative_attr_prob <- table(myData[,qualitative_attr])/nrow(myData)

# Calculate the join probability of target and qualitative_attr
joint_prob <- table(myData[,target], myData[,qualitative_attr])/nrow(myData)

# Calculate the conditional probability of target given qualitative_attr
conditional_prob <- NULL
for (i in seq_len(nrow(joint_prob))) {
  conditional_prob[i] <- joint_prob[i]/qualitative_attr_prob[i]
}

# Calculate the conditional probability of qualitative_attr given target
conditional_prob2 <- NULL
for (i in seq_len(nrow(joint_prob))) {
  conditional_prob2[i] <- joint_prob[i]/target_prob[i]
}

# Show the results
print("--------------------------------------------------")
print("target_prob")
print(target_prob)
print("--------------------------------------------------")
print("qualitative_attr_prob")
print(qualitative_attr_prob)
print("--------------------------------------------------")
print("joint_prob")
print(joint_prob)
print("--------------------------------------------------")
print("conditional_prob")
print(conditional_prob)
print("--------------------------------------------------")
print("conditional_prob2")
print(conditional_prob2)

# Calculate the conditiona probability of qualitative_attr given target with bayes theorem
conditional_prob3 <- NULL
for (i in seq_len(nrow(joint_prob))) {
  conditional_prob3[i] <- (joint_prob[i]*target_prob[i])/qualitative_attr_prob[i]
}
# Show the results
print("--------------------------------------------------")
print("conditional_prob3")
print(conditional_prob3)

# -------------------------------------
# 4. Part 4
# -------------------------------------
# Get the quantitative attributes
quantitative_attrs <- c("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours", "time_spend_company", "Work_accident", "promotion_last_5years")

# Compute the mean and variance of each quantitative attribute
for (attr in quantitative_attrs) {
  print(attr)
  print(mean(myData[,attr]))
  print(var(myData[,attr]))
}

# Compute the mean and variance of each quantitative attribute grouped by the target value
for (attr in quantitative_attrs) {
  print(attr)
  print(tapply(myData[,attr], myData[,target], mean))
  print(tapply(myData[,attr], myData[,target], var))
}

# Order the variables in terms of their importance by computing the class conditional mean difference scaled by the standard deviation
for (attr in quantitative_attrs) {
  print(attr)
  print((mean(myData[myData[,target]==1,attr])-mean(myData[myData[,target]==0,attr]))/sqrt((var(myData[myData[,target]==1,attr])+var(myData[myData[,target]==0,attr]))/2))
}

# -------------------------------------
# 5. Part 5
# -------------------------------------
# Choose one quantitative attribute f
f <- "satisfaction_level"

# Write down the normal distribution for the following parameters : P(f),P(f|y) explain what changes between the different distributions
# P(f) = N(0.61, 0.06)
# P(f|y) = N(0.44, 0.10)

# Plot theses normal distributions, compare them to the respective histograms and comment
# P(f)
hist(myData[,f], freq=FALSE, breaks=20, col="blue", main="Histogram of satisfaction_level", xlab="satisfaction_level")
curve(dnorm(x, mean=0.61, sd=sqrt(0.06)), add=TRUE, col="red", lwd=2)
# P(f|y)
hist(myData[myData[,target]==1,f], freq=FALSE, breaks=20, col="blue", main="Histogram of satisfaction_level", xlab="satisfaction_level")
curve(dnorm(x, mean=0.44, sd=sqrt(0.10)), add=TRUE, col="red", lwd=2)

# -------------------------------------
# 6. Part 6
# -------------------------------------
# For each qualitative attribute, visualize the probability distribution and the conditional probabilities by bar charts
for (attr in qualitative_attrs) {
  # Probability distribution
  barplot(table(myData[,attr])/nrow(myData), main=paste("Probability distribution of", attr), xlab=attr, ylab="Probability")
  # Conditional probabilities
  barplot(table(myData[,attr], myData[,target])/nrow(myData), main=paste("Conditional probabilities of", target, "given", attr), xlab=attr, ylab="Probability")
}

# For each quantitative attribute, visualize the distribution and the conditional distributions by histograms
for (attr in quantitative_attrs) {
  # Distribution
  hist(myData[,attr], freq=FALSE, breaks=20, col="blue", main=paste("Histogram of", attr), xlab=attr)
  # Conditional distributions
  hist(myData[myData[,target]==1,attr], freq=FALSE, breaks=20, col="blue", main=paste("Histogram of", attr, "given", target), xlab=attr)
}

# Which attributes seem the most and the least useful to predict the target value ? Why ?
# The most useful attributes are the quantitative attributes, because they are more discriminant than the qualitative attributes.
# The least useful attributes are the qualitative attributes, because they are not discriminant.

# Pick two continuous variables you think are the most useful and visualize their effect on the target in a scatter plot
# satisfaction_level and last_evaluation
plot(myData[,c("satisfaction_level", "last_evaluation")], col=myData[,target]+1, main="Scatter plot of satisfaction_level and last_evaluation", xlab="satisfaction_level", ylab="last_evaluation")
