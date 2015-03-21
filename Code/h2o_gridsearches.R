#h2o gridsearch


library(h2o)
localH2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '3g', nthreads = -1)
setwd("/Users/timkreienkamp/documents/studium/data_science/machine_learning//ML-Competition")
data <- read.csv("./data/Kaggle_Covertype_training.csv")[,2:56]


###### preprocessing
for (i in 1:10){
  data[,i] <- as.numeric(data[,i])
}

data[,55] <- as.factor(data[,55])

data$EDTH <- data$elevation - data$hor_dist_hyd*0.2
data$EVTH <-  data$elevation - data$ver_dist_hyd

data$tot_dist_hyd <- sqrt((data$hor_dist_hyd)^2 + (data$ver_dist_hyd)^2)

#store column indices in variables
initial_features <- 1:54
new_features <- 56:58

#############

#convert data to h2o format
covertype.hex <- as.h2o(localH2o,data, key = "covertype.hex")

#set parameters for grid searches

# for deep learning

# activation function
activation <- c("Tanh", "Rectifier")
#regularization
l_1 <- c(0,1e-5, 1e-3)
#hidden layers
hidden <- list(c(100,100,100),c(200,200,200), c(100,100,100,100), c(200,200,200,200))

#parameters for random forest

n_trees <- c(40,80,100)
depth <- c(8, 16)


#parameters for gbm

boost_iter <- c(40, 60, 120)

gbm.depth <- c(5,7)


grid_search_deep <- h2o.deeplearning(x=c(initial_features), y=55, data=covertype.hex, nfolds = 10,
                                hidden=hidden, epochs=15,
                                activation=activation, l1=l_1)

grid_search_rf <- h2o.randomForest(x = c(initial_features), 
                                   y = 55, 
                                   ntree = n_trees,
                                   data = covertype.hex,
                                   nfolds = 10,
                                   depth = depth,
                                   sample.rate = 0.9,
                                   importance = T
                                   )

grid_search_gbm <- h2o.gbm(x = c(initial_features),
                           data = covertype.hex,
                           y = 55, 
                           n.trees = boost_iter,
                           interaction.depth = gbm.depth,
                           nfolds = 10
                           )

grid_search_deep_2 <- h2o.deeplearning(x=c(initial_features, new_features), y=55, data=covertype.hex, nfolds = 10,
                                     hidden=hidden, epochs=15,
                                     activation=activation, l1=l_1)

grid_search_rf_2 <- h2o.randomForest(x = c(initial_features, new_features), 
                                   y = 55, 
                                   ntree = n_trees,
                                   data = covertype.hex,
                                   nfolds = 10,
                                   depth = depth,
                                   sample.rate = 0.9,
                                   importance = T, 
                                   nfolds = 10
)

grid_search_gbm_2 <- h2o.gbm(x = c(initial_features, new_features),
                           data = covertype.hex,
                           y = 55, 
                           n.trees = boost_iter,
                           interaction.depth = gbm.depth,
                           nfolds = 10
)





