#set up packages
if (!require("h2o")) install.packages("h2o")
library(h2o)
# start h2o
localH2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '30g', nthreads = -1)

###############################################################
#1. load and manipulate the Data
###############################################################



#setwd("/users/timkreienkamp/documents/studium/data_science/machine_learning/ML-Competition/")
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
data$Hydro_Road_1 <- abs(data$hor_dist_hyd + data$hor_dist_road)
data$Fire_Road_1 <- abs(data$hor_dist_fire +data$hor_dist_road)
data$hydro_road_2 <-  abs(data$hor_dist_hyd - data$hor_dist_road)
data$Fire_Road_2 <- abs(data$hor_dist_fire - data$hor_dist_road)


data$soil_type_15 <- NULL

#load the data into h2o
covertype.hex <- as.h2o(localH2o, data, key = "covertype.hex")



###############################################################
#2. Recursive Feature Elimination
###############################################################




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
    
    write.csv(best_features, "./results/best_features_rfe_rf.csv")
    write.csv(results_rfe, "./results/results_rfe_rf.csv")
  }
}

#collect results

results_rfe <- data.frame(m_vec, error_vec)
names(results_rfe) <- c("NoFeatures", "Error")

#save results
write.csv(results_rfe, "./results/results_rfe_rf.csv")



###############################################################
#3. New Grid search with best features
###############################################################




trees <- c(100, 150, 200)
depth <- c(20, 40, 100)

rf_grid_15_features <-  h2o.randomForest(x = feature_list[[24]], #names of feature columns
                          y = 54, data = covertype.hex, 
                          nfolds = 10, 
                          depth = depth,
                          sample.rate = 0.95, 
                          
                          seed = 123,
                          type = "BigData"
                          ntree = trees)


rf_grid_17_features <- h2o.randomForest(x = feature_list[[23]], #names of feature columns
                                        y = 54, data = covertype.hex, 
                                        nfolds = 10, 
                                        depth = depth,
                                        sample.rate = 0.95, 
                                        seed = 123,
                                        type = "BigData",
                                        ntree = trees)

rf_grid_19_features <- h2o.randomForest(x = feature_list[[22]], #names of feature columns
                                        y = 54, data = covertype.hex, 
                                        nfolds = 10, 
                                        depth = depth,
                                        sample.rate = 0.95, 
                                        seed = 123,
                                        type = "BigData",
                                        ntree = trees)

rf_grid_29_features <- h2o.randomForest(x = feature_list[[17]], #names of feature columns
                                        y = 54, data = covertype.hex, 
                                        nfolds = 10, 
                                        depth = depth,
                                        sample.rate = 0.95,                                         
                                        seed = 123,
                                        type = "BigData",
                                        ntree = trees)


best_depth <- rf_grid_17_features@model[[1]]@model$params$depth
best_n_trees <-  rf_grid_17_features@model[[1]]@model$params$ntree


rf_grid_17_features_max_depth <- h2o.randomForest(x = feature_list[[23]], #names of feature columns
                                        y = 54, data = covertype.hex, 
                                        nfolds = 10, 
                                        #mtries = tries,
                                        sample.rate = 0.95, 
                                        importance = T,
                                        seed = 123,
                                        depth = 100,
                                        ntree = best_n_trees, 
                                        type = "BigData")

rf_grid_17_features_best_depth <- h2o.randomForest(x = feature_list[[23]], #names of feature columns
                                                  y = 54, data = covertype.hex, 
                                                  nfolds = 10, 
                                                  #mtries = tries,
                                                  sample.rate = 1.0,
                                                  seed = 123,
                                                  depth = best_depth,
                                                  ntree = best_n_trees, 
                                                  type = "BigData")

rf_grid_17_features_depth_100 <- h2o.randomForest(x = feature_list[[23]], #names of feature columns
                                                   y = 54, data = covertype.hex, 
                                                   nfolds = 10, 
                                                   #mtries = tries,
                                                   sample.rate = 1.0,
                                                   seed = 123,
                                                   depth = 100,
                                                   ntree = best_n_trees, 
                                                   type = "BigData")


#collect errors in data frame
best_15_features_error <- rf_grid_15_features@model[[1]]@model$confusion[8,8]
rf_grid_17_features_error <-  rf_grid_17_features@model[[1]]@model$confusion[8,8]
rf_grid_29_features_error  <-  rf_grid_29_features@model[[1]]@model$confusion[8,8]

results_best_features <- data.frame(c(15,17,29), c(best_15_features_error, rf_grid_17_features_error, rf_grid_29_features_error) )
names(results_best_features) <- c("NoFeatures", "Error")

error_best_depth <- rf_grid_17_features_best_depth@model$confusion[8,8]

# write results to csv
write.csv(results_best_features, "./results/results_best_features.csv")
write.csv(error_best_depth, "./results/error_best_depth.csv")

#retrain best model on full data

rf_17_features_full <- h2o.randomForest(x = feature_list[[23]], #names of feature columns
                                        y = 54, data = covertype.hex, 
                                         
                                        #mtries = tries,
                                        sample.rate = 1.0,
                                        importance = T,
                                        seed = 123,
                                        depth = best_depth,
                                        ntree = best_n_trees, 
                                        type = "BigData")

#######################################################
#4. load test data and predict
#######################################################

test_data <- read.csv("./data/Kaggle_Covertype_test.csv")
test_ids <- test_data[,1]
test <- test_data[,2:55]

for (i in 1:10){
  test[,i] <- as.numeric(test[,i])
}


#prepare features

test$EDTH <- test$elevation - test$hor_dist_hyd*0.2
test$EVTH <-  test$elevation - test$ver_dist_hyd
test$tot_dist_hyd <- sqrt((test$hor_dist_hyd)^2 + (test$ver_dist_hyd)^2)
test$Hydro_Road_1 <- abs(test$hor_dist_hyd + test$hor_dist_road)
test$Fire_Road_1 <- abs(test$hor_dist_fire + test$hor_dist_road)
test$hydro_road_2 <-  abs(test$hor_dist_hyd - test$hor_dist_road)
test$Fire_Road_2 <- abs(test$hor_dist_fire - test$hor_dist_road)

test$soil_type_15 <- NULL

#put data in h2o
test.hex <- as.h2o(localH2o, test, key = "test.hex")

#predict
test_preds <- h2o.predict(rf_grid_17_features_max_depth, test.hex)
# to dataframe
test_preds <- as.data.frame(test_preds)

# write submission for the depth 100 model with sample.rate = 0.95
submission_17_features_max_depth <- data.frame(test_ids, test_preds$predict)
names(submission_17_features_max_depth) <- c("id", "Cover_Type")
write.csv(submission_17_features_max_depth, "./Submissions/submission_17_features_max_depth.csv", row.names = F)


#write submission for the best model retrained on the full dataset

############## THIS IS THE FINAL SUBMISSION ######################
test_preds_rf_full <- h2o.predict(rf_17_features_full, test.hex)
test_preds_rf_full <- as.data.frame(test_preds_rf_full)
submission_rf_full <- data.frame(test_ids, test_preds_rf_full$predict)
names(submission_rf_full) <- c("id", "Cover_Type")
write.csv(submission_rf_full, "./submissions/submission_rf_full_17_features.csv", row.names = F)
