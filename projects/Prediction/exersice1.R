library(HistData)
library(dplyr)
library(caret)
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

Sigma

dat %>% head()

# We will build 100 linear models using the data above 
#and calculate the mean and standard deviation of the combined models. 

# First, set the seed to 1 again (make sure to use sample.kind="Rounding" if your R is version 3.6 or later). 
# Then, within a replicate() loop, 
# (1) partition the dataset into test and training sets with p=0.5 and using dat$y to generate your indices,
# (2) train a linear model predicting y from x, 
# (3) generate predictions on the test set, and 
# (4) calculate the RMSE of that model. 
# Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.

set.seed(1, sample.kind="Rounding")

RMSE <- replicate(n, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm( y~x, data =train_set)
    yHat <- predict(fit, test_set)
    sqrt(mean((yHat - test_set$y)^2))
})

length(RMSE)
mean(RMSE)
sd(RMSE)


func <- function(n) {
    Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
    dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
      data.frame() %>% setNames(c("x", "y"))

    RMSE <- replicate(100, {
      test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
      train_set <- dat %>% slice(-test_index)
      test_set <- dat %>% slice(test_index)
      fit <- lm( y~x, data =train_set)
      yHat <- predict(fit, test_set)
      sqrt(mean((yHat - test_set$y)^2))
    })
    
    c(mean(RMSE),sd(RMSE))
}

set.seed(1, sample.kind="Rounding")

n <- c(100, 500, 1000, 5000, 10000)
result <- sapply(n, func)


result


set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

Sigma

dat %>% head()

cor(dat)


set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

