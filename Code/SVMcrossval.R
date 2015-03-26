#=============================================================================#
#SVM with no cross-valdation over 10 folds, but just 1 test and train
#=============================================================================#
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("foreach")) install.packages("foreach")
if (!require("kernlab")) install.packages("kernlab")
if (!require("e1071")) install.packages("e1071")
library("kernlab"); library("e1071");library(foreach);library(ggplot2);library(dplyr)

if (!require("snow")) install.packages("snow")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("parallel")) install.packages("parallel")
library("parallel"); library("doSNOW"); library("snow")



#==================================================
#run in amazon
noCores<-3
cl <- makeCluster(noCores, type="SOCK", outfile="")
registerDoSNOW(cl)


#=============================================================================#
#reading data
#=============================================================================#
setwd("C:/Users/Jéssica/Dropbox/machinelearningproject")
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
#g<-svm(Y~., data=training, type="C-classification", scale=F)
#accuracyg<- sum(predict(g,xtest)==ytest)/length(ytest)
#60% of accuracy: small compared if we play with hyperparameters
#=============================================================================#


#=============================================================================#
#useful link: http://scikit-learn.org/stable/modules/svm.html
#http://cran.r-project.org/web/packages/e1071/vignettes/svmdoc.pdf
#=============================================================================#
L <- 10 #L: data points to delete
nobs <- dim(data)[1]
idL <- rep(1:L, each=ceiling(nobs/L))  #ceiling: rounding up
idL <- idL[sample(1:nobs)] #random distribution of repeat 1 to 10 each one 600 times
#it is 600 times since it is the same as n/10 which is the number of points that are gonna be deleted
idL <- idL[1:nobs]
data <- data %>% mutate(bucketid=idL) 


cost<- c(0.1, 0.2, 0.4, 0.8, 1, 2, 5)
Llist <- rep(1:L, length(cost)) 
costlist <- rep(cost, L) 

svm_cost<- foreach(bucket = Llist, coost = costlist, .combine=rbind, .packages=c("kernlab", "dplyr")) %dopar% {
  xtrain <- data %>% filter(bucketid != bucket) %>% select(-bucket, -Y)
  ytrain <- data %>% filter(bucketid != bucket) %>% select(Y)
  ytrain<-as.factor(as.matrix(ytrain[,1]))
  xtrain<-as.matrix(xtrain)
  xtest <- data %>% filter(bucketid == bucket) %>% select(-bucket, -Y) 
  ytest <- data %>% filter(bucketid == bucket) %>% select(Y) 
  ytest<-as.factor(ytest[,1])
  model_svm<- ksvm(x=xtrain, y=ytrain,type="spoc-svc",kernel="rbfdot",scaled=F,kpar=list(sigma=0.1),C=coost)
  ypred<-predict(model_svm,as.matrix(xtest))
  obs<-as.numeric(ytest)
  pred<-as.numeric(ypred)
  accuracy<-sum(pred==obs)/length(ytest)  
  r<- cbind(coost, accuracy)
  colnames(r)<-c("Cost value","Accuracy")
  r
}
colnames(svm_cost) <- c("C", "Accuracy")
acc_svm_cost<-as.data.frame(svm_cost) %>% group_by(C) %>% summarize(cvAc=mean(Accuracy))
write.csv(acc_svm_cost, file = "results/SVM_cost.csv")




costsvm<-read.csv("results/SVM_cost.csv", sep=",",header=T)
colnames(costsvm)<-c("Cost","Accuracy")
#png("costsvm.png")
ggplot(data=costsvm, aes(x=Cost, y=Accuracy))+geom_line(size=1, col="darkred")+xlab("Cost values")+
  ylab("Cross validation Accuracy")+
  theme(panel.background = element_rect(fill = 'white', colour = 'grey'))+
  theme(plot.title = element_text(lineheight=.8, face="bold"))
#dev.off()



bestCbigC<-svm_cost[max(svm_cost$Accuracy),1]


#===============================
sigma<- seq(0.1,1,0.1)
List <- rep(1:L, length(sigma)) 
sigmalist <- rep(sigma, L) 

svm_sigma<- foreach(bucket = List, sig = sigmalist, .combine=rbind, .packages=c("kernlab", "dplyr")) %dopar% {
  xtrain <- data %>% filter(bucketid != bucket) %>% select(-bucket, -Y)
  ytrain <- data %>% filter(bucketid != bucket) %>% select(Y)
  ytrain<-as.factor(as.matrix(ytrain[,1]))
  xtrain<-as.matrix(xtrain)
  xtest <- data %>% filter(bucketid == bucket) %>% select(-bucket, -Y) 
  ytest <- data %>% filter(bucketid == bucket) %>% select(Y) 
  ytest<-as.factor(ytest[,1])
  model_svm<- ksvm(x=xtrain, y=ytrain,type="spoc-svc",kernel="rbfdot",scaled=F,kpar=list(sigma=sig),C=bigC)
  ypred<-predict(model_svm,as.matrix(xtest))
  obs<-as.numeric(ytest)
  pred<-as.numeric(ypred)
  accuracy<-sum(pred==obs)/length(ytest)  
  r<- cbind(coost, accuracy)
  colnames(r)<-c("Cost value","Accuracy")
  r
}
colnames(svm_sigma) <- c("S", "Accuracy")
acc_svm_sigma<-as.data.frame(svm_sigma) %>% group_by(S) %>% summarize(cvAc=mean(Accuracy))
write.csv(acc_svm_sigma, file = "SVM_sigma.csv")


sigmasvm<-read.csv("results/SVM_sigma.csv", sep=",",header=T)
colnames(sigmasvm)<-c("sigma","Accuracy")
#png("sigmasvm.png")
ggplot(data=sigmasvm, aes(x=sigma, y=Accuracy))+geom_line(size=1, col="darkred")+xlab("Sigma values")+
  ylab("Cross validation Accuracy")+
  theme(panel.background = element_rect(fill = 'white', colour = 'grey'))+
  theme(plot.title = element_text(lineheight=.8, face="bold"))
#dev.off()