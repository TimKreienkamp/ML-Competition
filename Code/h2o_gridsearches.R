#h2o gridsearch


library(h2o)
localH2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '30g', nthreads = -1)
data <- read.csv("Kaggle_Covertype_training.csv")[,2:56]

data$soil_type_15 = NULL


################################################################
#preprocessing
################################################################
=======
data <- read.csv("./data/Kaggle_Covertype_training.csv")[,2:56]


for (i in 1:10){
  data[,i] <- as.numeric(data[,i])
}

data[,54] <- as.factor(data[,54])

################################################################
#feature creation
################################################################


data$EDTH <- data$elevation - data$hor_dist_hyd*0.2
data$EVTH <-  data$elevation - data$ver_dist_hyd
data$aspect_shifted <- ifelse(data$aspect > 180, data$aspect - 180 , data$aspect)
data$tot_dist_hyd <- sqrt((data$hor_dist_hyd)^2 + (data$ver_dist_hyd)^2)
data$Hydro_Road_1 <- abs(data$hor_dist_hyd + data$hor_dist_road)
data$Fire_Road_1 <- abs(data$hor_dist_fire +data$hor_dist_road)
data$hydro_road_2 <-  abs(data$hor_dist_hyd - data$hor_dist_road)
data$Fire_Road_2 <- abs(data$hor_dist_fire - data$hor_dist_road)

data$tot_dist_hyd <- sqrt((data$hor_dist_hyd)^2 + (data$ver_dist_hyd)^2)

#store column indices in variables
initial_features <- 1:53
new_features <- 55:57

#############

#convert data to h2o format
covertype.hex <- as.h2o(localH2o,data, key = "covertype.hex")

#set parameters for grid searches

# for deep learning

# activation function
activation <- c("Tanh", "Rectifier")
#regularization
l_1 <- c(0,1e-5, 1e-3)
#hidden layers / units
hidden <- list(c(200,200,200), c(200,200,200,200))

#parameters for random forest

n_trees <- c(40,80,100, 150, 200)


#parameters for gbm

boost_iter <- c(40, 60, 120)

gbm.depth <- c(3, 5,7)

# run the deep learning grid search
grid_search_deep <- h2o.deeplearning(x=c(initial_features), y=54, data=covertype.hex, nfolds = 10,
                                hidden=hidden, epochs=15,
                                activation=activation, l1=l_1)

# save model to disk to inspect it locally, i.e. while not paying for AWS
h2o.saveModel(grid_search_deep@model[[1]], name = "deep_gridsearch_best", dir = "/home/rstudio", save_cv = TRUE, force=FALSE)
best_deep <- h2o.loadModel(localH2o, "deep_gridsearch_best")


# run the random forest grid search
grid_search_rf <- h2o.randomForest(x = c(initial_features), 
                                   y = 54, 
                                   ntree = n_trees,
                                   data = covertype.hex,
                                   nfolds = 10, 
                                   sample.rate = 0.9,
                                   importance = T
                                   )
#save model to disk 
h2o.saveModel(grid_search_rf@model[[1]], name = "rf_gridsearch_best", dir = "/home/rstudio", save_cv = TRUE, force=FALSE)


grid_search_gbm <- h2o.gbm(x = c(initial_features),
                           data = covertype.hex,
                           y = 54, 
                           n.trees = boost_iter,
                           interaction.depth = gbm.depth,
                           nfolds = 10
                           )
#save model to disk
h2o.saveModel(grid_search_gbm@model[[1]], name = "gbm_gridsearch_best", dir = "/home/rstudio", save_cv = TRUE, force=FALSE)
best_gbm <- h2o.loadModel(localH2o, "gbm_gridsearch_best")


### collect accuracies of all models and save in dataframe for viz later

best_rf <- grid_search_rf@model[[1]]


accuracy_best_deep <- best_deep@model$confusion[8,8]
accuracy_best_rf <- best_rf@model$confusion[8,8]
accuracy_best_gbm <- best_gbm@model$confusion[8,8]

initial_models <- c("RF", "GBM", "Deep Learning")
accuracy <- c(accuracy_best_rf, accuracy_best_gbm, accuracy_best_deep)

accuracies_initial_models <- data.frame(initial_models, accuracy)
names(accuracies_initial_models) <- c("Hypothesis Class", "Accuracy")

write.csv(accuracies_initial_models, "accuracies_initial.csv" )

# get feature importances from random Forest
#names of the features
importances <- best_rf@model$varimp
#mean decrease in accuracy
importances_mean_decrease <- as.numeric(importances[1,])
importances_features <- names(importances)
# put in data frame and sort
importances <- data.frame(importances_features, importances_mean_decrease)
names(importances)  <- c("Feature", "MeanDecreaseAcc")
importances <- importances[order(-importances$MeanDecreaseAcc),]


#get indices of the 40 best features
best_40_features <- as.character(importances[1:40,1])

#save the dataframe to make a plot for the report
  
# now that we have a better idea of which features perform well let's feed those - and our new features
# into the deep learning and the random forest
  
deep_new_features <- h2o.deeplearning(x=c(initial_features, new_features), y=54, data=covertype.hex, nfolds = 10,
                                     hidden=c(200,200,200), # best from grid search
                                     epochs=15,
                                     activation="Tanh" # best from grid search
                                     )

rf_new_features <- h2o.randomForest(x = c(initial_features, new_features), 
                                   y = 54, 
                                   ntree = n_trees,
                                   data = covertype.hex,
                                  
                                   sample.rate = 0.9,
                                   importance = T, 
                                   nfolds = 10
                                   )
###collect results again
rf_new_features_acc <- rf_new_features@model$confusion[8,8]
dl_new_features_acc <- deep_new_features@model$confusion[8,8]

models_2 <- c("Random Forest", "Deep Learning")
accuracies_2 <- c(rf_new_features_acc, dl_new_features_acc)

accuracies_new_features <- data.frame(models_2, accuracies_2)
names(accuracies_new_features) <- c("Hypothesis Class", "Accuracy")
write.csv(accuracies_new_features, "accuracies_new_features.csv")

## deep features ####

deepfeatures_1 <- h2o.deepfeatures(covertype.hex, best_deep)
rf_deep_1 <- h2o.randomForest(x = 2:201, 
                                    y = 1, 
                                    ntree = n_trees,
                                    data = deepfeatures_1,
                                    
                                    sample.rate = 0.9,
                                    importance = T, 
                                    nfolds = 10
)
best_rf_deep <- rf_deep_1@model[[1]]
h2o.saveModel(best_rf_deep, dir = "/home/rstudio")

accuracy_rf_deep <- best_rf_deep@model$confusion[8,8]
data.frame(accuracy_rf_deep)
write.csv(accuracy_rf_deep, "accuracy_rf_deep")




# deep learning with 20, 40, 60 neurons

hidden <- list(c(200, 200, 200, 60), c(200,200, 200, 40), c(200,200,200,20))

deep_several_neurons <- h2o.deeplearning(x=c(initial_features, new_features), y=54, data=covertype.hex, nfolds = 10,
                                      hidden=hidden, 
                                      epochs=15,
                                      activation="Tanh" # best from grid search
)

best_deep_several_neurons <- deep_several_neurons@model[[1]]
best_deep_several_neurons@model$params$hidden


h2o.saveModel(deep_several_neurons@model[[1]], "deep_60_neurons")
h2o.saveModel(deep_several_neurons@model[[2]], "deep_40_neurons")
h2o.saveModel(deep_several_neurons@model[[3]], "deep_20_neurons")
h2o.saveModel(rf_model, "rf_60_neuros")

deep_60_neurons <- h2o.loadModel(localH2o,"/home/rstudio/deep_60_neurons/Deep60Neuros")
deep_40_neurons <- h2o.loadModel(localH2o, "/home/rstudio/deep_40_neurons/deep_40_neurons")
deep_20_neurons <- h2o.loadModel(localH2o, "/home/rstudio/deep_20_neurons/deep_20_neurons")

deepfeatures_2 <- h2o.deepfeatures(covertype.hex, deep_60_neurons)
deepfeatures_40_neurons <- h2o.deepfeatures(covertype.hex, deep_40_neurons)
deepfeatures_20_neurons <- h2o.deepfeatures(covertype.hex, deep_20_neurons)


rf_60_neurons <- h2o.randomForest(y = 1, x = 2:61, data = deepfeatures_2, nfolds = 10, ntree = 100, sample.rate = 0.9)
rf_40_neurons <- h2o.randomForest(y = 1, x = 2:41, data = deepfeatures_40_neurons, nfolds = 10, ntree = 100, sample.rate = 0.9)
rf_20_neurons <- h2o.randomForest(y = 1, x = 2:21, data = deepfeatures_20_neurons, nfolds = 10, ntree = 100, sample.rate = 0.9)


#save error to csv
rf_60_neurons_error <- rf_60_neurons@model$confusion[8,8][1]
rf_40_neurons_error <- rf_40_neurons@model$confusion[8,8][1]
rf_20_neurons_error <- rf_20_neurons@model$confusion[8,8][1]

neurons_last_layer <- c(60,40,20)
error_vec <- c(rf_60_neurons_error, rf_40_neurons_error, rf_20_neurons_error)
error_comparison_neurons <- data.frame(neurons_last_layer, error_vec)
names(error_comparison_neurons) <- c("HiddenUnitsLastLayer", "CvError")
write.csv(error_comparison_neurons, "error_comparison_neurons.csv")


#load test data and predict

test <- read.csv("Kaggle_Covertype_test.csv")
test_ids <- test[,1]




for (i in 1:10){
  test[,i] <- as.numeric(test[,i])
}

test$EDTH <- test$elevation - test$hor_dist_hyd*0.2
test$EVTH <-  test$elevation - test$ver_dist_hyd

test$tot_dist_hyd <- sqrt((test$hor_dist_hyd)^2 + (test$ver_dist_hyd)^2)
test$soil_type_15 <- NULL
test.hex <- as.h2o(localH2o,test[2:57], key = "covertype_test.hex")

deepfeatures_60_neurons_test <- h2o.deepfeatures(test.hex, deep_60_neurons)
deepfeatures_40_neurons_test <- h2o.deepfeatures(test.hex, deep_40_neurons)

predictions.hex <- h2o.predict(rf_60_neurons, deepfeatures_60_neurons_test)
predictions_40_neurons <- h2o.predict(rf_40_neurons, deepfeatures_40_neurons_test)

predictions <- as.data.frame(predictions.hex$predict)
predictions_40_neurons <-  as.data.frame(predictions_40_neurons$predict)

submission_3 <- cbind(test_ids, predictions)
submission_40_neurons <- cbind(test_ids, predictions_40_neurons)

names(submission_3) <- c("id", "Cover_Type")
names(submission_40_neurons) <- c("id", "Cover_Type")

write.csv(submission_3, "submission_3.csv", row.names = F)
write.csv(submission_40_neurons, "submission_40_neurons.csv", row.names = F)

