

# setup
#install.packages("caret")
library(caret)
library(scales)
library(dplyr)


# total distance to water
train$tot_dist_hyd <- sqrt((train$hor_dist_hyd)^2 + (train$ver_dist_hyd)^2)

# colors
z.cols <- cut(train$Cover_Type, 7, labels = c("pink", "green", "yellow", "blue", "red","green4", "darkmagenta"))

# vertical vs. horizontal
plot(train$hor_dist_hyd, train$ver_dist_hyd, col = alpha(as.character(z.cols), 0.1), pch = 16)

# vertical vs. horizontal by group
plot(subset(train$hor_dist_hyd, train$Cover_Type = 1) ,
     subset(train$ver_dist_hyd, train$Cover_Type = 1 )
#   col = alpha(as.character(z.cols), 0.2), pch = 16)


plot(train$hor_dist_hyd, train$ver_dist_hyd, col = alpha(as.character(z.cols), 0.2), pch = 16)

head(train)

stripchart(train$tot_dist_hyd)


