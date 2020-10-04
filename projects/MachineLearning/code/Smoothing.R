library(dslabs)
library(dplyr)
library(tidyverse)
data("polls_2008")

str(polls_2008)

head(polls_2008)

?polls_2008

qplot(day, margin, data = polls_2008)

fit<-lm(margin ~ day, data = polls_2008)

new_margin = predict.lm(fit, data = polls_2008)

qplot(polls_2008$day, new_margin )



span <- 7

fit <- with(polls_2008, 
             ksmooth(day,margin,x.points = day, kernel = "box", bandwidth = span))

fit_1 <- with(polls_2008, 
            ksmooth(day,margin,x.points = day, kernel = "normal", bandwidth = span))


polls_2008 %>% mutate(smooth_1 = fit$y, smooth_2 = fit_1$y) %>% 
  ggplot(aes(day,margin)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day,smooth_1), color = "red") +
  geom_line(aes(day,smooth_2), color = "blue")



str(fit)

fit

span <- 25

fit_20 <- with(polls_2008, 
            ksmooth(day,margin,x.points = day, kernel = "box", bandwidth = span))



polls_2008 %>% mutate(smooth_1 = fit$y, smooth_2 = fit_20$y) %>% 
              ggplot(aes(day,margin)) +
              geom_point(size = 3, alpha = 0.5, color = "grey") +
              geom_line(aes(day,smooth_1), color = "red") +
              geom_line(aes(day,smooth_2), color = "blue")



  
totalDays <- diff(range(polls_2008))

totalDays
span <- 21/totalDays

span


fit_loess <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)

# An iterative algorithm is implemented in which,
# after fitting a model in one iteration, outliers are detected and down-weighted
fit_loess_sym <- loess(margin ~ day, degree = 1, span = span, data = polls_2008, family = "symmetric")


polls_2008 %>% mutate(smooth_1 = fit_loess$fitted, smooth_2 = fit_loess_sym$fitted) %>% 
  ggplot(aes(day,margin)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day,smooth_1), color = "red") +
  geom_line(aes(day,smooth_2), color = "blue") 


fit_loess <- loess(margin ~ day, degree = 2, span = span, data = polls_2008)

polls_2008 %>% mutate(smooth_1 = fit$y, smooth_2 = fit_20$y,smooth_fit = fit_loess$fitted, smooth_2 = fit_20$y) %>% 
  ggplot(aes(day,margin)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day,smooth_1), color = "red") +
  geom_line(aes(day,smooth_2), color = "blue") +
  geom_line(aes(day,smooth_fit), color = "black")



polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method.args = list(degree=1))


polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method.args = list(degree=1))


