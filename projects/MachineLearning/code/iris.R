
# We will practice building a machine learning algorithm using a new dataset,
# iris, that provides multiple predictors for us to use to train. To start,
# we will remove the setosa species and we will focus on the versicolor and
# virginica iris species using the following code:

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding")    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]


nrow(test)
nrow(train)
length(y)

data.frame(cutoff,perf) %>% ggplot(aes(cutoff,perf)) + geom_line()
cutoff[which.max(perf)]

names(train)

train %>% summarize(min(Sepal.Length)
                    ,max(Sepal.Length)
                    ,min(Sepal.Width)
                    ,max(Sepal.Width)
                    ,min(Petal.Length)
                    ,max(Petal.Length)
                    ,min(Petal.Width)
                    ,max(Petal.Width))

# min(Sepal.Length) max(Sepal.Length) min(Sepal.Width) max(Sepal.Width) min(Petal.Length)
# 1     5.1               7.9              2.3              3.8                 3
# max(Petal.Length) min(Petal.Width) max(Petal.Width)
# 1     6.9                1              2.5

SepLenCutoff <- seq(5.1,7.9,0.1)
perf <- sapply(SepLenCutoff, function(x) {
  yHatNew <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% factor(levels = levels(train$Species))
  mean(yHatNew == train$Species)
})

data.frame(SepLenCutoff,perf) %>% ggplot(aes(SepLenCutoff,perf)) + geom_line()
SepLenCutoff[which.max(perf)]
max(perf)
# 0.74
# > SepLenCutoff[which.max(perf)]
# [1] 6.2


SepWidCutoff <- seq(2.3,3.8,0.1)
perf <- sapply(SepWidCutoff, function(x) {
  yHatNew <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% factor(levels = levels(train$Species))
  mean(yHatNew == train$Species)
})

data.frame(SepWidCutoff,perf) %>% ggplot(aes(SepWidCutoff,perf)) + geom_line()
SepWidCutoff[which.max(perf)]
max(perf)

# > SepWidCutoff[which.max(perf)]
# [1] 2.9
# > max(perf)
# [1] 0.64


PetLenCutoff <- seq(3,6.9,0.1)
perf <- sapply(PetLenCutoff, function(x) {
  yHatNew <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% factor(levels = levels(train$Species))
  mean(yHatNew == train$Species)
})

data.frame(PetLenCutoff,perf) %>% ggplot(aes(PetLenCutoff,perf)) + geom_line()
PetLenCutoff[which.max(perf)]
max(perf)

# > PetLenCutoff[which.max(perf)]
# [1] 4.8
# > max(perf)
# [1] 0.96


PetWidCutoff <- seq(1,2.5,0.1)
perf <- sapply(PetWidCutoff, function(x) {
  yHatNew <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% factor(levels = levels(train$Species))
  mean(yHatNew == train$Species)
})

data.frame(PetWidCutoff,perf) %>% ggplot(aes(PetWidCutoff,perf)) + geom_line()
PetWidCutoff[which.max(perf)]
max(perf)

# > PetWidCutoff[which.max(perf)]
# [1] 1.7
# > max(perf)
# [1] 0.98


foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}

nrow(train[,-5])

predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	

predictions

y_hat <- ifelse(x>i,'virginica','versicolor')
mean(y_hat==train$Species)


yHatNew <- ifelse(test$Petal.Length > 4.7, "virginica", "versicolor") %>% factor(levels = levels(test$Species))
mean(yHatNew == test$Species)


predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)


foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	



plot(iris,pch=21,bg=iris$Species)




yHatNew <- ifelse(test$Petal.Width > 1.7| test$Petal.Length > 4.7  , "virginica", "versicolor") %>% factor(levels = levels(test$Species))
mean(yHatNew == test$Species)


library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5


y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)


width_cutoff
length_cutoff
