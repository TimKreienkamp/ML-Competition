#=============================================================================#
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("snow")) install.packages("snow")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if (!require("parallel")) install.packages("parallel")
if (!require("randomForest")) install.packages("randomForest")
library("ggplot2")
library("parallel")
library("dplyr")
library("doSNOW")
library("foreach")
library("snow")
library("dplyr")
library(class)
#=============================================================================#




#=============================================================================#
#reading data
#=============================================================================#
setwd("~/GitHub/ML-Competition")
train<-read.csv("Data/Kaggle_Covertype_training.csv", sep=",",header=T)
data<-train[,2:56] # we do not care about the id info 



#=============================================================================#
#scaling of the numeric variables
#=============================================================================#
for (i in 1:10){
  data[,i] <- as.numeric(data[,i])
  data[,i]<-(data[,i]-mean(data[,i]))/(sd(data[,i]))
}


#=============================================================================#
#categorical data
#=============================================================================#
for (i in 11:54){
  data[,i] <- as.factor(data[,i])
}
#we will consider two cases for the column 55, Y


#=============================================================================#
######### for paralellization
noCores <- detectCores()-1
cl <- makeCluster(noCores, type="SOCK", outfile="")
registerDoSNOW(cl)
#=============================================================================#


#=============================================================================#
#10kfoldcrossvalidation
#=============================================================================#
L <- 10 #L: data points to delete
nobs <- dim(data)[1]
idL <- rep(1:L, each=ceiling(nobs/L))  #ceiling: rounding up
idL <- idL[sample(1:nobs)] #random distribution of repeat 1 to 10 each one 600 times
#it is 600 times since it is the same as n/10 which is the number of points that are gonna be deleted
idL <- idL[1:nobs] 
#=============================================================================#


#=============================================================================#
#mutating with respect to the randomly sampled id of buckets
#=============================================================================#
data <- data %>% mutate(bucketid=idL) 
#Mutate adds new variables and preserves existing
#we create the same data information but now considering the order of idL
colnames(data)[55] <- "Y"




#=============================================================================#
#Y as integer
#=============================================================================#
data[,55]<-as.integer(data[,55])

k <- seq(1,200,1)
Llist <- rep(1:L, length(k)) #repeat 3 times (k) 1:L
klist <- rep(k, L) #repeast 10 times l

resnotfactor<- foreach(bucket = Llist, k = klist, .combine=rbind, .packages=c("class", "dplyr")) %dopar% {
  xtrain <- data %>% filter(bucketid != bucket) %>% select(-bucket, -Y)
  ytrain <- data %>% filter(bucketid != bucket) %>% select(Y)
  
  xtest <- data %>% filter(bucketid == bucket) %>% select(-bucket, -Y) #testing on the eliminated data
  ytest <- data %>% filter(bucketid == bucket) %>% select(Y) # we take the Y values from the eliminated previously data points
 
  testpredictions <- knn(train=xtrain, test=xtest, cl=ytrain$Y, k=k) #we apply the KNN on that testing part with k nneighbours
  testerror <- mean(testpredictions != ytest$Y) #we calculate the errors (predictions different from true values) made by our classifier (trained previously and tested with new data) on the points in the deleted block 
  res <- c(bucket, k, testerror) #the result if the L eliminated points, the k neighbours and the errors made
}
colnames(resnotfactor) <- c("testBucket", "k", "Error")
errorcrossvalidation_notfactor<-as.data.frame(resnotfactor) %>% group_by(k) %>% summarize(cvError=mean(Error))
errorcrossvalidation_notfactor

write.csv(errorcrossvalidation_notfactor, file = "errorcrossvalidation_notfactor.csv")
stopCluster(cl)




#=============================================================================#
#Y as  factor 
#=============================================================================#
data[,55]<-as.factor(data[,55])


resfactor<- foreach(bucket = Llist, k = klist, .combine=rbind, .packages=c("class", "dplyr")) %dopar% {
  xtrain <- data %>% filter(bucketid != bucket) %>% select(-bucket, -Y)
  ytrain <- data %>% filter(bucketid != bucket) %>% select(Y)
  
  xtest <- data %>% filter(bucketid == bucket) %>% select(-bucket, -Y) #testing on the eliminated data
  ytest <- data %>% filter(bucketid == bucket) %>% select(Y) # we take the Y values from the eliminated previously data points
  
  testpredictions <- knn(train=xtrain, test=xtest, cl=ytrain$Y, k=k) #we apply the KNN on that testing part with k nneighbours
  testerror <- mean(testpredictions != ytest$Y) #we calculate the errors (predictions different from true values) made by our classifier (trained previously and tested with new data) on the points in the deleted block 
  res <- c(bucket, k, testerror) #the result if the L eliminated points, the k neighbours and the errors made
}
colnames(resfactor) <- c("testBucket", "k", "Error")
errorcrossvalidation_factor<-as.data.frame(resfactor) %>% group_by(k) %>% summarize(cvError=mean(Error))

write.csv(errorcrossvalidation_factor, file = "errorcrossvalidation_factor.csv")



#=============================================================================#
######plotting results
#=============================================================================#
notfactor<-read.csv("results/errorcrossvalidation_notfactor.csv", sep=",",header=T)
png("images/ECV_notfactor.png")
ggplot(data=notfactor, aes(x=k, y=cvError))+geom_line(size=1, col="darkblue")+xlab("K values")+
  ylab("Cross validation Error")+ggtitle("Cross-Validation Error\nwhen Y is an integer")+
  theme(panel.background = element_rect(fill = 'antiquewhite', colour = 'grey'))+
  theme(plot.title = element_text(lineheight=.8, face="bold"))
dev.off()
#minimum for: notfactor[notfactor$cvError==min(notfactor[,3]),] => 3 k


factor<-read.csv("results/errorcrossvalidation_factor.csv", sep=",",header=T)
png("images/ECV_factor.png")
ggplot(data=factor, aes(x=k, y=cvError))+geom_line(size=1, col="darkred")+xlab("K values")+
  ylab("Cross validation Error")+ggtitle("Cross-Validation Error\nwhen Y is a factor")+
  theme(panel.background = element_rect(fill = 'antiquewhite', colour = 'grey'))+
  theme(plot.title = element_text(lineheight=.8, face="bold"))
dev.off()
#factor[factor$cvError==min(factor[,3]),]  #minimum also with 3
