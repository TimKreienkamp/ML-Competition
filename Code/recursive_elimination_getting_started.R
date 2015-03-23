install.packages("h2o")
library(h2o)
# on the AWS set max_mem_size = 30g and nthreads = -1
localH2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '3g', nthreads = 4)

setwd("/users/timkreienkamp/documents/studium/data_science/machine_learning/ML-Competition/")
data <- read.csv("./data/Kaggle_Covertype_training.csv")[,2:56]


for (i in 1:10){
data[,i] <- as.numeric(data[,i])
}

#encode target as factor
data[,55] <- as.factor(data[,55])

### feature engineering ######

data$EDTH <- data$elevation - data$hor_dist_hyd*0.2
data$EVTH <-  data$elevation - data$ver_dist_hyd
data$tot_dist_hyd <- sqrt((data$hor_dist_hyd)^2 + (data$ver_dist_hyd)^2)

data$soil_type_15 <- NULL

#load the data into h2o
covertype.hex <- as.h2o(localH2o, data[1:1000,], key = "covertype.hex")


#train a random forest 

rf_1 <-  h2o.randomForest(x = c(1:53, 55:57), #indices or names of feature columns
                          y = 54, covertype.hex, 
                          nfolds = 10, 
                          importance = T,
                          ntree = 100) # error = 0.14

#get the importances -- bigger is better

feat_importances <- rf_1@model$varimp

# get the error
rf_1@model$confusion[8,8]

# get a data frame with feature names and corresponding importances
feature_names <- names(rf_1@model$varimp)
mean_decrease_accuracy <- as.numeric(feat_importances[1,])
feature_importances <- data.frame(feature_names, mean_decrease_accuracy)

# order in descending order
feature_importances <- feature_importances[order(-feature_importances$mean_decrease_accuracy),]

#get the names of the 50 most important features 

most_important <- as.character(feature_importances$feature_names[1:50])


#train a new random forest with those features

rf_2 <-  h2o.randomForest(x = most_important, #indices or names of feature columns
                          y = 54, covertype.hex, 
                          nfolds = 10, 
                          importance = T,
                          ntree = 100) 


