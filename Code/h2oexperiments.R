
install.packages("h2o")
library(h2o)
localH2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '3g')
setwd("/users/timkreienkamp/documents/studium/data_science/machine_learning/ML-Competition/")
data <- read.csv("./data/Kaggle_Covertype_training.csv")[,2:56]


# do some minor preprocessing -- note that I am NOT scaling the features here because it is not required for RF - for SVM it is mandatory!
#convert integer fields to numeric
for (i in 1:10){
  data[,i] <- as.numeric(data[,i])
}

#encode binary variables explicitly as factors
for (i in 11:55){
  data[,i] <- as.factor(data[,i])
}

covertype.hex <- as.h2o(localH2o, data, key = "covertype.hex")

deep <- h2o.deeplearning(x = 1:54, y = 55, covertype.hex, nfolds = 10, epochs = 20) #error : 0.169

rf <- h2o.randomForest(x = 1:54, y = 55, covertype.hex, nfolds = 10, ntree = 225) # error = 0.14

rf_2 <- h2o.randomForest(x = 1:54, y = 55, covertype.hex, nfolds = 10, ntree = 225, sample.rate = 0.9) #0.138


gbm <- h2o.gbm(x = 1:54, y = 55, distribution = "multinomial", covertype.hex, n.trees = 100, nfolds = 10) #0.189

hidden <- c(50,50,50,50)

deep_2 <- h2o.deeplearning(x = 1:54, y = 55, covertype.hex, hidden = hidden, nfolds = 10, epochs = 20)  #0.181


deepfeatures_1 <- h2o.deepfeatures(covertype.hex, deep)

rf_df <- h2o.randomForest(x = 2:201, y = 1, deepfeatures_1, nfolds = 10, ntree = 225, sample.rate = 0.9) 


hidden_2 <- c(200,200,200)
deep_3 <- h2o.deeplearning(x = 1:54, y = 55, covertype.hex, hidden = hidden_2, nfolds = 10, epochs = 20)  # 0.15

deepfeatures_2 <- h2o.deepfeatures(covertype.hex, deep_3)

rf_df_2 <- h2o.randomForest(x = 2:201, y = 1, deepfeatures_2, nfolds = 10, ntree = 225, sample.rate = 0.9) # 0.09 



test_data <- read.csv("./data/Kaggle_Covertype_test.csv")
test_ids <- test_data[,1]
test_X <- test_data[,2:55]

for (i in 1:10){
  test_X[,i] <- as.numeric(test_X[,i])
}

#encode binary variables explicitly as factors
for (i in 11:54){
  test_X[,i] <- as.factor(test_X[,i])
}



test.hex <- as.h2o(localH2o, test_X, key = "test.hex")

deepfeatures_test <- h2o.deepfeatures(test.hex, deep_3)


test_preds <- h2o.predict(rf_df_2, deepfeatures_test)

test_preds <- as.data.frame(test_preds)

submission_2_df_rf <- data.frame(test_ids, test_preds$predict)
names(submission_2_df_rf) <- c("id", "Cover_Type")

write.csv(submission_2_df_rf, "./data/submission_2_df_rf.csv", row.names = F)
