#install.packages("h2o")
library(h2o)
# on the AWS set max_mem_size = 30g and nthreads = -1
localH2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '30g', nthreads = -1)

#setwd("/users/timkreienkamp/documents/studium/data_science/machine_learning/ML-Competition/")
data <- read.csv("Kaggle_Covertype_training.csv")[,2:56]


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
data$Hydro_Road_1 <- abs(data$hor_dist_hyd + data$hor_dist_road)
data$Fire_Road_1 <- abs(data$hor_dist_fire +data$hor_dist_road)
data$hydro_road_2 <-  abs(data$hor_dist_hyd - data$hor_dist_road)
data$Fire_Road_2 <- abs(data$hor_dist_fire - data$hor_dist_road)


data$soil_type_15 <- NULL

#load the data into h2o
covertype.hex <- as.h2o(localH2o, data, key = "covertype.hex")


### set up data structures####

feature_list <- list()
conf_mats <- list()
error_vec <- c()
m_vec <- c()


#start with all features, make sure to replace the 57 by the last column index you have
most_important <- c(names(data[,1:53]), names(data[,55:62]))

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
                            seed = 1234,
                            ntree = 100)
  error_vec <- c(error_vec, rf_1@model$confusion[8,8])
  conf_mats[[i]] <- rf_1@model$confusion
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
write.csv(results_rfe, "results_rfe_rf.csv")


# do a new grid search for the best features 



trees <- c(80, 100, 120, 150, 200)

rf_grid_15_features <-  h2o.randomForest(x = feature_list[[24]], #names of feature columns
                          y = 54, data = covertype.hex, 
                          nfolds = 10, 
                          
                          sample.rate = 0.95, 
                          importance = T,
                          seed = 123,
                          ntree = 100)


rf_grid_17_features <- h2o.randomForest(x = feature_list[[23]], #names of feature columns
                                        y = 54, data = covertype.hex, 
                                        nfolds = 10, 
                                        #mtries = tries,
                                        sample.rate = 0.95, 
                                        importance = T,
                                        seed = 123,
                                        ntree = trees)

rf_grid_19_features <- h2o.randomForest(x = feature_list[[22]], #names of feature columns
                                        y = 54, data = covertype.hex, 
                                        nfolds = 10, 
                                        #mtries = tries,
                                        sample.rate = 0.95, 
                                        importance = T,
                                        seed = 123,
                                        ntree = trees)

rf_grid_29_features <- h2o.randomForest(x = feature_list[[17]], #names of feature columns
                                        y = 54, data = covertype.hex, 
                                        nfolds = 10, 
                                        #mtries = tries,
                                        sample.rate = 0.95, mtries = tries,
                                        importance = T,
                                        seed = 123,
                                        ntree = trees)

rf_37_features <- h2o.randomForest(x = feature_list[[13]], #names of feature columns
                                   y = 54, data = covertype.hex, 
                                   nfolds = 10,
                                   #mtries = tries,
                                   sample.rate = 0.95, 
                                   importance = T,
                                   seed = 123,
                                   ntree = trees)

rf_grid_17_features_max_depth <- h2o.randomForest(x = feature_list[[23]], #names of feature columns
                                        y = 54, data = covertype.hex, 
                                        nfolds = 10, 
                                        #mtries = tries,
                                        sample.rate = 0.95, 
                                        importance = T,
                                        seed = 123,
                                        depth = 100,
                                        ntree = 200, 
                                        type = "BigData")

best_15_features_error <- rf_grid_15_features@model[[1]]@model$confusion[8,8]
best_15_features_trees <- rf_grid_15_features@model[[1]]$ntree
rf_grid_17_features_error <-  rf_grid_17_features@model[[1]]@model$confusion[8,8]
best_17_features_trees <- rf_grid_17_features@model[[1]]$ntree
rf_grid_29_features_error  <-  rf_grid_29_features@model[[1]]@model$confusion[8,8]
best_29_features_trees <- rf_grid_29_features@model[[1]]@model$params$ntree
rf_37_features_error <-  rf_37_features@model[[1]]@model$confusion[8,8]
best_37_features_trees <- rf_37_features@model[[1]]@model$params$ntree
rf_grid_17_features_max_depth_error <- rf_grid_17_features_max_depth@model$confusion[8,8]



####predict#####
test_data <- read.csv("Kaggle_Covertype_test.csv")
test_ids <- test_data[,1]
test <- test_data[,2:55]

for (i in 1:10){
  test[,i] <- as.numeric(test[,i])
}



test$EDTH <- test$elevation - test$hor_dist_hyd*0.2
test$EVTH <-  test$elevation - test$ver_dist_hyd
test$tot_dist_hyd <- sqrt((test$hor_dist_hyd)^2 + (test$ver_dist_hyd)^2)
test$Hydro_Road_1 <- abs(test$hor_dist_hyd + test$hor_dist_road)
test$Fire_Road_1 <- abs(test$hor_dist_fire + test$hor_dist_road)
test$hydro_road_2 <-  abs(test$hor_dist_hyd - test$hor_dist_road)
test$Fire_Road_2 <- abs(test$hor_dist_fire - test$hor_dist_road)

test$soil_type_15 <- NULL

test.hex <- as.h2o(localH2o, test, key = "test.hex")

test_preds <- h2o.predict(rf_grid_17_features_max_depth, test.hex)

test_preds <- as.data.frame(test_preds)

submission_17_features_max_depth <- data.frame(test_ids, test_preds$predict)
names(submission_17_features_max_depth) <- c("id", "Cover_Type")
write.csv(submission_17_features_max_depth, "submission_17_features_max_depth.csv", row.names = F)

