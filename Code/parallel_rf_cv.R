#parallel cross validation - RandomForest

# we are going to check the 10-fold cv performance of random forests, for different values of 
#n.trees (the number of trees in the forest) and sampsize 
#(size of sample to draw)

# the cross validation itself starts at line 58

setwd("/users/timkreienkamp/documents/studium/data_science/machine_learning/ML-Competition/")


#firest load all the libraries we need
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("snow")) install.packages("snow")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("parallel")) install.packages("parallel")
if (!require("randomForest")) install.packages("randomForest")
library("ggplot2")
library("randomForest")
library("parallel")
library("dplyr")
library("doSNOW")
library("foreach")
library("snow")

#register the cluster


#if you are doing this on your windows machine leave one core for facebook; 
#if you are using an EC2 instance max out performance and use all available cores by removing
# the "-1"
noCores <- detectCores()-1

cl <- makeCluster(noCores, type="SOCK", outfile="") 
registerDoSNOW(cl)

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

#some useful values
noObs <- dim(data)[1]
noFeats <- dim(data)[2]

#######cross validation######

#set the number of cv Buckets
noBuckets <- 10

#hyperparameters we want to optimize over --- be careful, the lists with the underscores are the short
# ones and the lists without are the expanded ones
ntree_List <- c(50, 75, 100, 125, 150, 175, 200, 225, 250)
sampsize_List <- c(0.8, 0.85, 0.9, 0.95, 1)

# bucket indicator
idx <- rep(1:noBuckets, each=ceiling(noObs/noBuckets))  
idx <- idx[sample(1:noObs)]
idx <- idx[1:noObs]  # if it is an odd number

# adding the variable
#data <- cbind(y,X)
data <- data %>% mutate(bucketId=idx)
colnames(data)[55] <- "Y"

#set up the grid
bucketList <- rep(1:noBuckets, length(ntree_List)*length(sampsize_List))
ntreeList <- rep(ntree_List, (noBuckets*length(sampsize_List)))
sampsizeList <- rep(sampsize_List[1], (length(ntreeList)/length(sampsize_List)))
for (i in 2:length(sampsize_List)){
  sampsizeList <- c(sampsizeList, rep(sampsize_List[i], (length(ntreeList)/length(sampsize_List))))
}

#start the parallel cv
results<- foreach(bucket = bucketList, ntree = ntreeList, sampsize = sampsizeList,
                  .combine=rbind, .packages=c("randomForest", "dplyr")) %dopar% {
                    
                    # some helpful debugging messages
                    cat("Bucket", bucket, "is the current test set! ntree=", ntree, "sampsize = ", sampsize)
                    
                    # subsetting the training phase data
                    Xtrain <- data %>% filter(bucketId != bucket) %>% select(-bucket, -Y)
                    Ytrain <- data %>% filter(bucketId != bucket) %>% select(Y)
                    
                    
                    
                    
                    # subsetting the test phase data
                    Xtest <- data %>% filter(bucketId == bucket) %>% select(-bucket, -Y)
                    Ytest <- data %>% filter(bucketId == bucket) %>% select(Y)
                    
                    
                    
                    # train RF
                    
                    cat(str(Ytrain$Y))
                    rf_fit <- randomForest(Xtrain, Ytrain$Y, ntree = ntree, replace = F, sampsize = floor(sampsize*length(Ytrain$Y)))
                    trainPredictions <- predict(rf_fit, Xtrain)
                    trainError <- mean(trainPredictions != Ytrain$Y)
                    testPreds <- predict(rf_fit, Xtest)
                    cvError <- mean(testPreds != Ytest$Y)
                    
                    # last thing is returned
                    result <- c(bucket, ntree, sampsize, trainError, cvError)
                  }
stopCluster(cl)

results <- as.data.frame(results)
names(results) <- c("bucket", "ntree", "sampsize", "trainError", "cvError")

results %>% group_by(sampsize,ntree) %>% summarize(Error=mean(cvError))
