library(dslabs)
library(dplyr)
data(heights)
class(heights$height)


class("Male")
class(75.000)

nrow(heights)

heights$height[777]

heights[777,1]

max(heights$height)



which.min(heights$height)
mean(heights$height)
median(heights$height)




mean(heights$sex == "Male")

sum(heights$height > 78)

sum(heights$height > 78 & heights$sex == "Female")

# Caret package, training and test sets, and overall accuracy

install.packages("caret")

library(caret)
