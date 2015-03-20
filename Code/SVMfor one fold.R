#=============================================================================#
#SVM with no cross-valdation over 10 folds, but just 1 test and train
#=============================================================================#
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
svmm<-list()
ypred<-list()
accuracy<-list()
maxcost<-20
for(i in 1:maxcost){
  svmm[[i]] <- svm(Y~.,data=training,scale=F,kernel="radial",cost=i)
  ypred[[i]] = predict(svmm[[i]],xtest)
  accuracy[[i]]<-sum(ypred[[i]]==ytest)/length(ytest)  

}
results_1svm<-cbind(seq(1,maxcost,1),as.matrix(accuracy))
colnames(results_1svm)<-c("Cost of SVM", "Accuracy")
write.csv(results_1svm, file = "results/SVM_1split_accuracy.csv")
