library(caret)

set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  
  f_0 <- rnorm(n, mu_0, sigma_0)
  print(mu_1)
  f_1 <- rnorm(n, mu_1, sigma_1)
  
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()


set.seed(1, sample.kind="Rounding")

mu_1 <- seq(0, 3, len=25)

dat_25 <- sapply(mu_1, make_data, mu_1 = mu_1)

seq(1:25)

res <- sapply(seq(1:25),function(x1) {
fit_glm <- glm(y ~ x, data=dat_25[1,x1]$train, family = "binomial")
p_hat_glm <- predict(fit_glm, dat_25[2,x1]$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
confusionMatrix(data = y_hat_glm, reference = dat_25[2,x1]$test$y)$overall["Accuracy"]
})


res_2 <- sapply(seq(2:25),function(x) {
  fit_glm <- glm(y ~ x, data=dat_25[,x]$train, family = "binomial")
  p_hat_glm <- predict(fit_glm, dat_25[,x]$test)
  y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
  confusionMatrix(data = y_hat_glm, reference = dat_25[,x]$test$y)$overall["Accuracy"]
})

mu_2 <- seq(0.125, 3, len=24)
mu_2

plot(mu_1,res)

which.max(res_2)

mu_1[]



dat_25 <- sapply(seq(0, 3, len=25), function (x){
  make_data(mu_1 = x)
  })


dat_25[,1]$train


dat_25[1,25]$train


set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  nrow(dat$train)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
  nrow(dat$train)
})
qplot(delta, res)