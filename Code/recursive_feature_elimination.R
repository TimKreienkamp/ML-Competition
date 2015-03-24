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
data$aspect_shifted <- ifelse(data$aspect > 180, data$aspect - 180 , data$aspect)

data$soil_type_15 <- NULL

#load the data into h2o
covertype.hex <- as.h2o(localH2o, data[1:1000,], key = "covertype.hex")


### set up data structures####

feature_list <- list()
error_vec <- c()
m_vec <- c()


#start with all features, make sure to replace the 57 by the last column index you have
most_important <- c(names(data[,1:53]), names(data[,55:57]))

m <- length(most_important)

# add a counter
i <- 1

#start the loop
while (m > 8){
  m_vec <- c(m_vec, m)
  feature_list[[i]] <- most_important
  rf_1 <-  h2o.randomForest(x = most_important, #names of feature columns
                            y = 54, covertype.hex, 
                            nfolds = 10, 
                            importance = T,
                            ntree = 100)
  error_vec <- c(error_vec, rf_1@model$confusion[8,8])
  print(rf_1@model$confusion[8,8])
  feature_names <- names(rf_1@model$varimp)
  mean_decrease_accuracy <- as.numeric(rf_1@model$varimp[1,])
  feature_importances <- data.frame(feature_names, mean_decrease_accuracy)
  feature_importances <- feature_importances[order(-feature_importances$mean_decrease_accuracy),]
  # cut off two at once
  most_important <- as.character(feature_importances$feature_names[1:(m-2)])
  m <- length(most_important)
  i = i+1
  # save every 5 iterations
  if(i %% 5  == 0 ) {
    results_rfe <- data.frame(m_vec, error_vec)
    names(results_rfe) <- c("NoFeatures", "Error")
    
    # get the best feature stack and save
    
    min_error_index <- which.min(error_vec)
    
    best_features <- feature_list[[min_error_index]]
    
    best_features <- data.frame(best_features)
    
    write.csv(best_features, "best_features_rfe_rf.csv")
    write.csv(results_rfe, "results_rfe_rf.csv")
  }
}

#collect results

results_rfe <- data.frame(m_vec, error_vec)
names(results_rfe) <- c("NoFeatures", "Error")

# get the best feature stack and save

min_error_index <- which.min(error_vec)

best_features <- feature_list[[min_error_index]]

best_features <- data.frame(best_features)

write.csv(best_features, "best_features_rfe_rf.csv")




