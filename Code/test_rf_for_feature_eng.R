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

###### HERE GOES THE FEATURE ENGINEERING PART##########


# convert to h2o data format

covertype.hex <- as.h2o(localH2o, 
                        data, #here goes the name of the dataframe you created, including all the features and the response column
                        key = "covertype.hex")

# train and cross validate random forest
rf <- h2o.randomForest(x = 1:54, #column indices of features
                       y = 55, # column index of response
                       covertype.hex, nfolds = 10, 
                       ntree = 225) 

# look at the overall cross validation error and see if it is better than 0.14
rf