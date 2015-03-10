library(dplyr)
library(ggplot2)
results_parallelRFVC <- read.csv("./data/parallel_RF_CV.csv")[,2:6]
results_grouped <- results_parallelRFVC %>% group_by(ntree,sampsize)
mean_cv_error <- summarize(results_grouped, mean(cvError))
names(mean_cv_error) <- c("ntree", "sampsize", "Error")
mean_cv_error$sampsize <- mean_cv_error$sampsize*100
mean_cv_error$sampsize <- as.character(mean_cv_error$sampsize)
ggplot(data = mean_cv_error, aes(x=ntree, y = Error, colour = sampsize, group = sampsize))+geom_line()
