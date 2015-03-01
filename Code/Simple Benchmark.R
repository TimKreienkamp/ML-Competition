setwd("/users/timkreienkamp/documents/studium/data_science/machine_learning/ML-Competition")
train <- read.csv("./Data/Kaggle_Covertype_training.csv")
library(randomForest)
train <- train[,2:56]
X_train <- train[,1:54]
y_train <- train[,55]
y_train <- as.factor(y_train)

for (i in 1:10){
  X_train[,i] <- as.numeric(X_train[,i])
}

for (i in 11:54){
  X_train[,i] <- as.factor(X_train[,i])
}

system.time(
rf_1 <- randomForest(X_train, y_train)
)


plot(rf_1$err.rate[,1])

