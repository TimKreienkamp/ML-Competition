setwd("/users/timkreienkamp/documents/studium/data_science/machine_learning/ML-Competition/")
library(ggplot2)



############################################################################################################################
#this contains all the visualizations for the report
# the order is chronological w.r.t to the paper, i.e. what shows up first in the paper shows up first here
############################################################################################################################


# first get together the results of the methods not used in h2o

#get the oob error of the very first forest as a baseline 

baseline <- read.csv("./results/oob_error_benchmark_rf.csv")[200,2]

results_knn <- read.csv("./results/knn_error.csv")[,2:3]

knn_best_error <- min(results_knn[,2])

results_svm <- read.csv("./results/SVM_1split_accuracy.csv")[,2:3]
results_svm$error <- 1- results_svm[,2]

svm_best_error <- min(results_svm$error)

# get h2o results
results_initial_h2o <- read.csv("./results/errors_initial.csv")[,2:3]

#glue together

errors_svm_knn <- data.frame(c("KNN", "SVM"), c(knn_best_error, svm_best_error))
names(errors_svm_knn) <- c("Hypothesis", "Error")
names(results_initial_h2o) <-  c("Hypothesis", "Error")
initial_features_errors <- rbind(results_initial_h2o, errors_svm_knn)


# draw the plot
barchart_initial_errors <- ggplot(data=initial_features_errors, 
                                  aes(x=Hypothesis, y=Error), colour = "green") + 
                                  geom_bar(stat="identity", fill = "blue", width = 0.5) + 
                                  geom_hline(yintercept = baseline, colour = "red", linetype = "dashed") +
                                  annotate("text", x = "RF", y = 0.26, label = "Baseline Error")

###############################################################
# now get the second block of errors 
################################################################

results_new_features <- read.csv("./results/errors_new_features.csv")[,2:3]
names(results_new_features) <- c("Hypothesis", "Error")

# best error from last block to serve as benchmark in this
best_error_initial_features <- min(initial_features_errors$Error)
best_hypothesis_initial_features <- as.character(initial_features_errors$Hypothesis[which.min(initial_features_errors$Error)])
  
barchart_new_features_errors <- ggplot(data=results_new_features, 
                                  aes(x=Hypothesis, y=Error), colour = "green") + 
                                  geom_bar(stat="identity", fill = "blue", width = 0.5) + 
                                  geom_hline(yintercept = baseline, colour = "red", linetype = "dashed") +
                                  geom_hline(yintercept = best_error_initial_features, colour = "black", linetype = "dashed")
                                  annotate("text", x= 2, y = 0.26, label = "Baseline Error")



  
  