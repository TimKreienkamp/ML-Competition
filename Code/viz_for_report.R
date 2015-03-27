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


# get h2o results
results_initial_h2o <- read.csv("./results/errors_initial.csv")[,2:3]

#glue together

errors_knn <- data.frame(c("KNN"), c(knn_best_error))
names(errors_knn) <- c("Hypothesis", "Error")
names(results_initial_h2o) <-  c("Hypothesis", "Error")
initial_features_errors <- rbind(results_initial_h2o, errors_knn)


# draw the plot
barchart_initial_errors <- ggplot(data=initial_features_errors, 
                                  aes(x=Hypothesis, y=Error), colour = "green") + 
                                  geom_bar(stat="identity", fill = "blue", width = 0.5) + 
                                  geom_hline(yintercept = baseline, colour = "red", linetype = "dashed") +
                                  annotate("text", x = "RF", y = 0.26, label = "Baseline Error")
ggsave("./doc/barchart_initial_errors.png")
dev.off()

###############################################################
# now get the second block of errors 
################################################################

results_new_features <- read.csv("./results/errors_new_features.csv")[,2:3]
names(results_new_features) <- c("Hypothesis", "Error")
rf_deepfeatures <- read.csv("./results/error_rf_deep")
rf_deepfeatures_error <- data.frame(c("Random Forest Deep Features"), rf_deepfeatures[1,2])
names(rf_deepfeatures_error) <- names(results_new_features)
results_new_features <- rbind(results_new_features,rf_deepfeatures_error )

# best error from last block to serve as benchmark in this
best_error_initial_features <- min(initial_features_errors$Error)
best_hypothesis_initial_features <- as.character(initial_features_errors$Hypothesis[which.min(initial_features_errors$Error)])
  
barchart_new_features_errors <- ggplot(data=results_new_features, 
                                  aes(x=Hypothesis, y=Error), colour = "green") + 
                                  geom_bar(stat="identity", fill = "blue", width = 0.5) + 
                                  geom_hline(yintercept = baseline, colour = "red", linetype = "dashed") +
                                  geom_hline(yintercept = best_error_initial_features, colour = "black", linetype = "dashed") 

ggsave("./doc/barchart_new_features_errors.png")
dev.off()

                                  

#################################################################
# errors for different numbers of neurons in the last hidden layer
##################################################################
  
  
error_neurons <-  read.csv("./results/error_comparison_neurons.csv")[,2:3]

barchart_error_neurons <- ggplot(data=error_neurons, 
                                       aes(x=HiddenUnitsLastLayer, y=CvError)) + 
                                        geom_bar(stat="identity", fill = "blue") + 
                                        geom_hline(yintercept = best_error_initial_features, colour = "black", linetype = "dashed")+
                                        annotate("text", x= 40, y = (best_error_initial_features+0.005), label = "Best Error Initial Features") + 
                                        ggtitle("Random Forest with Deep Learning Features")+
                                        xlab("Hidden Units in last Hidden Layer")
ggsave("./doc/barchart_error_neurons.png")
dev.off()

#################################################################
# results from the recursive feature elimination
##################################################################

results_rfe <-  read.csv("./results/results_rfe_rf.csv")[,2:3]


linechart_rfe <- ggplot(data=results_rfe, aes(x = NoFeatures, y = Error)) + geom_line()
ggsave("./doc/linechart_results_rfe.png")
dev.off() 

#################################################################
# results from final gridsearches
##################################################################

results_best_features <-  read.csv("./results/results_best_features.csv")[,2:3]

barchart_best_features <- ggplot(data=results_best_features, 
                                  aes(x=NoFeatures, y=Error)) + 
                                   geom_bar(stat="identity", fill = "blue")

ggsave("./doc/barchart_best_features.png")
dev.off()




