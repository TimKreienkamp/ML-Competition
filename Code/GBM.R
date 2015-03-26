if (!require("caret")) install.packages("caret")
library("caret") #boosting for classification

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("snow")) install.packages("snow")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("parallel")) install.packages("parallel")
library("ggplot2");library("parallel");library("dplyr");library("doSNOW");library("foreach");library("snow");library("dplyr");library(class)

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

round(diag(var(data[,11:54])),3)
newdata<-cbind(data[,1:20], data[,23:28],data[,30:49],data[,51:55])
#soil_type_7,8,15,36 has variance of 0 => eliminate them



######### for paralellization
noCores <- detectCores()-1
cl <- makeCluster(noCores, type="SOCK", outfile="")
registerDoSNOW(cl)
#=============================================================================#
L <- 10
nobs <- dim(newdata)[1]
idL <- rep(1:L, each=ceiling(nobs/L))  
idL <- idL[sample(1:nobs)] 
idL <- idL[1:nobs] 
newdata <- newdata %>% mutate(bucketid=idL) 
colnames(newdata)[51] <- "Y"

#===========================================

L<-10
shrin<- seq(1,1.2,0.1)
Llist <- rep(1:L, length(shrin)) 
shrinlist <- rep(shrin, L)


gbm<- foreach(bucket = Llist, shrin= shrinlist, .combine=rbind, .packages=c("caret", "dplyr")) %dopar% {
  xtrain <- newdata %>% filter(bucketid != bucket) %>% select(-bucket, -Y)
  ytrain <- newdata %>% filter(bucketid != bucket) %>% select(Y)
  training<-cbind(xtrain,ytrain)
    xtest <- newdata %>% filter(bucketid == bucket) %>% select(-bucket, -Y) #testing on the eliminated data
  ytest <- newdata %>% filter(bucketid == bucket) %>% select(Y) # we take the Y values from the eliminated previously data points
  
  tuning <-  expand.grid(n.trees = c(200),interaction.depth = 10 , shrinkage = shrin)
  gbm<- train(Y ~ .,data =training, method = "gbm", tuneGrid =tuning)
  pred<-predict(gbm, xtest); pred<-as.matrix(as.numeric(pred))
  obs<- as.matrix(as.numeric(ytest))
  accuracy<-sum(obs==pred)/length(obs)
  res <- c(shrin, accuracy)
  }
colnames(gbm) <- c("shrinkage", "accuracy")
cv_gbm<-as.data.frame(gbm) %>% group_by(shrinkage) %>% summarize(cvAc=mean(accuracy))

write.csv(cv_gbm, file = "cv_gbm.csv")
