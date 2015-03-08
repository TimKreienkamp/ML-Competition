setwd("/users/timkreienkamp/documents/studium/data_science/machine_learning/ML-Competition")
train <- read.csv("./Data/Kaggle_Covertype_training.csv")
library(randomForest)
train <- train[,2:56]
X_train <- train[,1:54]
y_train <- train[,55]
y_train <- as.factor(y_train)

#convert integer fields to numeric
for (i in 1:10){
  X_train[,i] <- as.numeric(X_train[,i])
}

#encode binary variables explicitly as factors
for (i in 11:54){
  X_train[,i] <- as.factor(X_train[,i])
}



rf_1 <- randomForest(X_train, y_train, ntree = 200, replace = F, sampsize = floor(0.99*50000))



plot(rf_1$err.rate[,1])

print(rf_1$err.rate[200,1])

error_rate <- data.frame(rf_1$err.rate)

#load test data
test <- read.csv("./Data/Kaggle_Covertype_test.csv")
test_ids <- test[,1]
test <- test[,2:55]


for (i in 1:10){
  test[,i] <- as.numeric(test[,i])
}


for (i in 11:54){
  test[,i] <- as.factor(test[,i])
}

preds <- predict(rf_1, test)

submission_rf_1 <- data.frame(test_ids)
submission_rf_1 <- cbind(submission_rf_1, preds)
names(submission_rf_1) <- c("id", "Cover_Type")
write.csv(submission_rf_1, "submission_1_rf_n_tree_200.csv", row.names = F)


