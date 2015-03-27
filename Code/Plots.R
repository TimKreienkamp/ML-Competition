if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
## loading data
setwd("/users/timkreienkamp/documents/studium/data_science/machine_learning/ML-Competition/")
data <- read.csv("./data/Kaggle_Covertype_training.csv")[,2:56]

# Dataset characteristics
### The dataset contains cartographic variables for 30 x 30 meter cells of forest within the Roosevelt National Forest of northern Colorado.   


#  Exploratory analysis
### The exploratory analysis is conducted to develop a thorough understanding of the dataset, to find relationships between the features and suggest new variables that might help to predict forrest cover type. Within this report we focus on relationships amoung features and the creation of new variables. A more elaborate feature exploration is presented in the "exploratory_analysis"  markdown file.

# Relation between features

## We use scatterplots to observe the relationships between the quantitative features.

## plotting elvation vs. vertical distance to hyd
ggplot(data, aes(x=elevation, y=ver_dist_hyd, colour=Cover_Type)) +
  geom_point(shape=1) +
  scale_colour_gradientn(colours=rainbow(4))


ggsave(file="./doc/elevation-ver_dist.png")
dev.off()
### there appears to be a strong relationship between the vertical distance and elevation within the groups. Thus we create a new variable to account for this relationship which will later be tested for its predictive power. 

## variable creation
data$EVTH <-  data$elevation - data$ver_dist_hyd

## plotting elvation vs. vertical distance to hyd
ggplot(data, aes(x=EVTH, y=ver_dist_hyd, colour=Cover_Type)) +
  geom_point(shape=1) +
  scale_colour_gradientn(colours=rainbow(4))

ggsave(file="./doc/EVTH-ver_dist.png")
dev.off()





