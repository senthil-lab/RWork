# Predict Sex from Height
#install.packages("e1071")
library(caret)
library(dslabs)
library(dplyr)
library(purrr)
data("heights")

set.seed(2)
y <- heights$sex
testIndex <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
# half # from males and half # from females
sum(y[testIndex] == "Male")
sum(y == "Male")
sum(y[testIndex] == "Female")
sum(y == "Female")
trainSet <- heights[-testIndex,]
testSet <- heights[testIndex,]

# Method 1 - Simple - Guessing ML algorithm : Sampling

yHat <- sample(c("Male","Female"), length(testIndex), replace = TRUE) %>%
        factor(levels = levels(testSet$sex))

mean(yHat == testSet$sex)


# on average, males are slightly taller than females.

heights %>% group_by(sex) %>% summarise(mean(height), sd(height))
# 
# sex    `mean(height)` `sd(height)`
# <fct>           <dbl>        <dbl>
#   1 Female           64.9         3.76
# 2 Male             69.3         3.61

# Predict male if height is within two standard deviations from the average male.

yHatNew <- ifelse(testSet$height> 62, "Male", "Female") %>% factor(levels = levels(testSet$sex))

mean(yHatNew == testSet$sex)

# we use the cutoff of 62 inches,
# but we can examine the accuracy obtained for other cutoffs
# and then take the value that provides the best result.

cutoff <- seq(61,70)
perf <- sapply(cutoff, function(x) {
              yHatNew <- ifelse(testSet$height > x, "Male", "Female") %>% factor(levels = levels(testSet$sex))
              mean(yHatNew == testSet$sex)
})

data.frame(cutoff,perf) %>% ggplot(aes(cutoff,perf)) + geom_line()
cutoff[which.max(perf)]


yHatNew <- ifelse(testSet$height> 64, "Male", "Female") %>% factor(levels = levels(testSet$sex))


table(pred = yHatNew, actu = testSet$sex)


testSet %>% mutate(y_hat = yHatNew) %>% group_by(sex) %>% summarize(mean(sex == y_hat))


confusionMatrix(data = yHatNew, reference = testSet$sex)

# Trying to evaluate the algorithm with F1 Score,

F_meas(data = yHatNew, reference = factor(testSet$sex))



cutoff <- seq(61,70)
perf <- sapply(cutoff, function(x) {
  yHatNew <- ifelse(trainSet$height > x, "Male", "Female") %>% factor(levels = levels(trainSet$sex))
  F_meas(data = yHatNew, reference = factor(trainSet$sex))
})

data.frame(cutoff,perf) %>% ggplot(aes(cutoff,perf)) + geom_line() + geom_point()
cutoff[which.max(perf)]

yHatNew <- ifelse(testSet$height> cutoff[which.max(perf)], "Male", "Female") %>% factor(levels = levels(testSet$sex))

testSet %>% mutate(y_hat = yHatNew) %>% group_by(sex) %>% summarize(mean(sex == y_hat))

confusionMatrix(data = yHatNew, reference = factor(testSet$sex))

# Confusion Matrix and Statistics
# 
# Reference
# Prediction Female Male
# Female     82   77
# Male       37  329
# 
# Accuracy : 0.7829          
# 95% CI : (0.7451, 0.8174)
# No Information Rate : 0.7733          
# P-Value [Acc > NIR] : 0.3221872       
# 
# Kappa : 0.4464          
# 
# Mcnemar's Test P-Value : 0.0002595       
#                                           
#             Sensitivity : 0.6891          
#             Specificity : 0.8103          
#          Pos Pred Value : 0.5157          
#          Neg Pred Value : 0.8989          
#              Prevalence : 0.2267          
#          Detection Rate : 0.1562          
#    Detection Prevalence : 0.3029          
#       Balanced Accuracy : 0.7497          
#                                           
#        'Positive' Class : Female   

sensitivity(data = yHatNew, reference = factor(testSet$sex))
specificity(data = yHatNew, reference = factor(testSet$sex))
# 
# > sensitivity(data = yHatNew, reference = factor(testSet$sex))
# [1] 0.6890756
# > specificity(data = yHatNew, reference = factor(testSet$sex))
# [1] 0.8103448



# ROC Curves between Guessing and Height Model

p <- 0.9
n <- length(testIndex)
yHat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(testSet$sex))
mean(yHat == testSet$sex)

probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  yHat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(yHat, testSet$sex),
       TPR = sensitivity(yHat, testSet$sex))
})

class(guessing)

guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  yHat <- ifelse(testSet$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(yHat, testSet$sex),
       TPR = sensitivity(yHat, testSet$sex))
})


height_cutoff %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

# comparing the Guessing Vs Prediction Using Height
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")


# precision-recall curves for Guessing Vs Prediction Using Height

probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  yHat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       precision = precision(yHat, testSet$sex),
       recall = sensitivity(yHat, testSet$sex))
})

class(guessing)

guessing %>% qplot(precision, recall, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  yHat <- ifelse(testSet$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       precision = precision(yHat, testSet$sex),
       recall = sensitivity(yHat, testSet$sex))
})

# comparing the Guessing Vs Prediction Using Height
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")


# Excercise:
# The following questions all ask you to work with the dataset described below.
# 
# The reported_heights and heights datasets were collected from three classes taught 
#in the Departments of Computer Science and Biostatistics, as well as remotely through 
#the Extension School. The Biostatistics class was taught in 2016 along with an online 
#version offered by the Extension School. On 2016-01-25 at 8:15 AM, during one of the 
#lectures, the instructors asked student to fill in the sex and height questionnaire 
#that populated the reported_heights dataset. The online students filled out the survey 
#during the next few days, after the lecture was posted online. We can use this insight 
#to define a variable which we will call type, to denote the type of student, inclass or 
#online.
# 
# The code below sets up the dataset for you to analyze in the following exercises:

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

length(x)

sum(x == "inclass")

length(y[which(x == "inclass")])

sum(y[which(x == "inclass")] == "Female")/length(y[which(x == "inclass")])


mean(y[which(x == "online")] == "Female")


yHatNew <- ifelse(x == "online", "Male", "Female") %>% factor(levels = levels(y))

mean(yHatNew == y)


head(data.frame(yHatNew, y))

table(predicted = y_hat, actual = y)


table(yHatNew,y)


sensitivity(data = yHatNew, reference = y)
specificity(data = yHatNew, reference = y)

confusionMatrix(data = yHatNew, reference = y)


cutoff <- seq(61,70)
perf <- sapply(cutoff, function(x) {
  yHatNew <- ifelse(testSet$height > x, "Male", "Female") %>% factor(levels = levels(testSet$sex))
  mean(yHatNew == testSet$sex)
})


library(dslabs)
data("heights")
heights %>%
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)



ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% 
mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)