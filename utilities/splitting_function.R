splitting<-function(data,k){
  index <- 1:nrow(data)
  testindex <- sample(index, trunc(length(index)/k))
  testset <- data[testindex, ]
  trainset <- data[-testindex, ]
  r<-list(trainset=trainset,testset=testset)
  r
}
