library(dslabs)
data(heights)
nrow(heights)
heights[777]
heights[1,777]
which.min(heights$height)
median(heights$height)
mean(heights$sex == "Male")
sum(heights$height > 78 & heights$sex == "Female")

# Caret package, training and test sets, and overall accuracy
install.packages("caret")

library(caret)
