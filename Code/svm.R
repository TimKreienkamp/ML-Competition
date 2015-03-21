if (!require("dplyr")) install.packages("dplyr")
if (!require("snow")) install.packages("snow")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("parallel")) install.packages("parallel")
if (!require("e1071")) install.packages("e1071")
library("parallel")
library("dplyr")
library("doSNOW")
library("foreach")
library("snow")
library("dplyr")
library("e1071")

#####
setwd("~/GitHub/ML-Competition")
train<-read.csv("Data/Kaggle_Covertype_training.csv", sep=",", header=T)
data<-train[,2:56]

#####
noCores <- detectCores()-1
cl <- makeCluster(noCores, type="SOCK", outfile="")
registerDoSNOW(cl)
###############
#numerical data
for (i in 1:10){
  data[,i] <- as.numeric(data[,i])
  data[,i]<- (data[,i]-mean(data[,i]))/(sd(data[,i]))
}
#categorical data
for (i in 11:55){
  data[,i] <- as.numeric(data[,i])
}


#training[training$Y==1,55]<-"A";training[training$Y==2,55]<-"B";training[training$Y==3,55]<-"C";training[training$Y==4,55]<-"D";training[training$Y==5,55]<-"E";training[training$Y==6,55]<-"F";training[training$Y==7,55]<-"G"
data[,55]<-as.factor(data[,55])

data<-data[1:100,]
###############crossvalidation
folds<-10
nobs <- dim(data)[1]
idfolds <- rep(1:folds, each=ceiling(nobs/folds))  
idfolds <- idfolds[sample(1:nobs)] 
idfolds <- idfolds[1:nobs] 

data <- data %>% mutate(foldsid=idfolds) 
colnames(data)[55] <- "Y"

#hyperparameter
cost <- c(1,2,3,4)
foldslist <- rep(1:folds, length(cost))
Clist <- rep(cost, folds) 



svmm<- foreach(f = foldslist, C = Clist, .combine=rbind, .packages=c("class", "dplyr")) %dopar% {
  xtrain <- data %>% filter(foldsid != f) %>% select(-f, -Y)
  ytrain <- data %>% filter(foldsid != f) %>% select(Y)
  train<-cbind(xtrain,ytrain)
  
  xtest <- data %>% filter(foldsid == f) %>% select(-f, -Y) 
  ytest <- data %>% filter(foldsid == f) %>% select(Y) 
  
  
  svm1 <- svm(Y~.,data=train,scale=F,kernel="radial",cost=C)
  ypred = predict(svm1,xtest)
  accuracy<-sum(ypred==ytest)/length(ytest)   
  res <- c(f, C, accuracy) 
  res
}
colnames(svmm) <- c("Fold", "Cost", "Accuracy")
accuracycrossvalidation_svm<-as.data.frame(svmm) %>% group_by(Cost) %>% summarize(Accuracy=mean(Accuracy))

write.csv(accuracycrossvalidation_svm, file = "accuracycrossvalidation_svm.csv")




