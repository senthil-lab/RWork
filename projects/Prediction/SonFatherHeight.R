library(HistData)
library(dplyr)
library(caret)
galton_height <- GaltonFamilies %>% 
                  filter(childNum == 1 & gender == "male") %>%
                  select(father, childHeight) %>%
                  rename(son = childHeight)

y <- galton_height$son

test_index <- createDataPartition(y, times = 1, p=0.5, list = FALSE)

train_set <- galton_height %>% slice(-test_index)
test_set <- galton_height %>% slice(test_index)

# just guess without including father's height
avg <- mean(train_set$son)
avg

# Mean squared loss
mean((avg - test_set$son)^2)

# As father and son height is bivariate normal distribution we can fit to a regression line
# b0 + b1 * x

fit <- galton_height %>% lm(son ~ father, data = .)
fit$coefficients

# (Intercept)      father 
# 35.7124909   0.5027904

yHat <- 35.7124909 + 0.5027904 * test_set$father

# Mean squared loss
mean((yHat - test_set$son)^2)

# uisng Predit Function
y_hat_withPredictFun <- predict(fit, test_set)
mean((y_hat_withPredictFun - test_set$son)^2)


class(y_hat_withPredictFun)

# library(HistData)
# set.seed(1983)
# galton_heights <- GaltonFamilies %>%
#   filter(gender == "male") %>%
#   group_by(family) %>%
#   sample_n(1) %>%
#   ungroup() %>%
#   select(father, childHeight) %>%
#   rename(son = childHeight)
# 
# y <- galton_heights$son
# test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
# train_set <- galton_heights %>% slice(-test_index)
# test_set <- galton_heights %>% slice(test_index)
# 
# m <- mean(train_set$son)
# # squared loss
# mean((m - test_set$son)^2)
# 
# # fit linear regression model
# fit <- lm(son ~ father, data = train_set)
# fit$coef
# y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
# mean((y_hat - test_set$son)^2)


