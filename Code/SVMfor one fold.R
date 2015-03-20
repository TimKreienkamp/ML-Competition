#=============================================================================#
#SVM with no cross-valdation over 10 folds, but just 1 test and train
#=============================================================================#
if (!require("dplyr")) install.packages("dplyr")
if (!require("snow")) install.packages("snow")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("parallel")) install.packages("parallel")
if (!require("kernlab")) install.packages("kernlab")
if (!require("e1071")) install.packages("e1071")

library("parallel")
library("dplyr")
library("doSNOW")
library("foreach")
library("snow")
library("dplyr")
library("kernlab")
library("e1071")



#=============================================================================#
#reading data
#=============================================================================#
setwd("~/GitHub/ML-Competition")
train<-read.csv("Data/Kaggle_Covertype_training.csv", sep=",", header=T)
data<-train[,2:56]

#=============================================================================#
for (i in 1:10){
  data[,i] <- as.numeric(data[,i])
  data[,i]<- (data[,i]-mean(data[,i]))/(sd(data[,i]))
}
#categorical data
for (i in 11:55){
  data[,i] <- as.numeric(data[,i])
}
data[,55]<-as.factor(data[,55])
colnames(data)[55]<-"Y"
#=============================================================================#

#=============================================================================#
#splitting the data into test and training part
#=============================================================================#
source("usefulthings/splitting_function.R")
training<-splitting(data,10)$trainset
testing<-splitting(data,10)$testset
xtrain<-training[,-55]
ytrain<-as.factor(training[,55])
xtest<-testing[,-55]
ytest<-as.factor(testing[,55])

#=============================================================================#
#g<-svm(Y~., data=training, type="C-classification", scale=F)
#accuracyg<- sum(predict(g,xtest)==ytest)/length(ytest)
#60% of accuracy: small compared if we play with hyperparameters
#=============================================================================#


#=============================================================================#
#useful link: http://scikit-learn.org/stable/modules/svm.html
#http://cran.r-project.org/web/packages/e1071/vignettes/svmdoc.pdf
#=============================================================================#
sizecost<-seq(1,2,0.5)
sizesigma<-seq(0.1,0.2,0.1)

results<- foreach(sigma =sizesigma, cost = sizecost, .combine=rbind, .packages=c("kernlab", "dplyr")) %dopar% {
model_svm<- ksvm(Y~.,data=training,kernel="rbfdot",scaled=F,kpar=list(sigma=sigma),C=cost,cross=10)
cat(".")
ypred = predict(model_svm,xtest)
accuracy<-sum(ypred==ytest)/length(ytest)  
r<- cbind(sigma, cost, accuracy)
colnames(r)<-c("Sigma value","Cost value","Accuracy")
r
}






