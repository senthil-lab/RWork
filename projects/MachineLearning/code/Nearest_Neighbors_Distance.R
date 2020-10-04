library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
#set.seed(1995) # if using R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]



x[500,783]

