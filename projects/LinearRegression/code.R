# install.packages("Lahman")
library(dslabs)
library(Lahman)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
ds_theme_set()
data("Teams")
str(Teams)
# Home Run Vs Total Run

Teams %>% filter(yearID %in% c(1961:2001)) %>% 
          mutate(hrPerGame = HR/G, rPerGame= R/G) %>%
          ggplot(aes(hrPerGame,rPerGame)) +
          geom_point(alpha = 0.5)

# Stolen bases Vs Total Run

Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(sbPerGame = SB/G, rPerGame= R/G) %>%
  ggplot(aes(sbPerGame,rPerGame)) +
  geom_point(alpha = 0.5)



# Bases on Balls Vs Total Run
Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(bbPerGame = BB/G, rPerGame= R/G) %>%
  ggplot(aes(bbPerGame,rPerGame)) +
  geom_point(alpha = 0.5)


Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(singlesPerGame = H/G, rPerGame= R/G, corr = cor(singlesPerGame,rPerGame)) %>%
  ggplot(aes(singlesPerGame,rPerGame)) +
  geom_point(alpha = 0.5)


Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(singlesPerGame = (H-HR-X2B-X3B)/G, rPerGame= R/G) %>% summarize( corr = cor(rPerGame,singlesPerGame)) %>% .$corr

# ?Teams


# Regression

#install.packages("HistData")
library(HistData)
data("GaltonFamilies")
galtonHeights <- GaltonFamilies %>% filter(childNum == 1 & gender == "male") %>%
                                    select(father, childHeight) %>%
                                    rename(son = childHeight)


hist(galtonHeights$son)

galtonHeights %>% summarize(sonMean = mean(son), sonSD = sd(son), 
                            fatherMean = mean(father), fatherSD = sd(father))


galtonHeights %>% ggplot(aes(father, son)) + geom_point()


# corelation between Son and Father heigh
galtonHeights %>% summarise(cor(father,son))

galtonHeights %>% summarise(cor(son, father))

nrow(galtonHeights)


ind <- sample(1:179,25)

set.seed(0)
sampleGaltonHeight <- sample_n(galtonHeights,25, replace = TRUE)
nrow(sampleGaltonHeight)

sampleGaltonHeight %>% summarise(cor(son, father))

B <- 1000
N <- 250
R <- replicate(B, {
  sampleGaltonHeight <- sample_n(galtonHeights, N, replace = TRUE)
  sampleGaltonHeight %>% summarise(r = cor(son, father)) %>% .$r
})

hist(R,20)

mean (R)
sd(R)


(72 - mean(galtonHeights$father)) / sd(galtonHeights$father)


galtonHeights_Father_72 <- GaltonFamilies %>% 
  filter(childNum == 1 & gender == "male" & father >= 71.5 & father <= 72.5) %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

nrow(galtonHeights_Father_72)

galtonHeights_Father_72


galtonHeights_Father_72 <- GaltonFamilies %>% 
  filter(childNum == 1 & gender == "male" & round(father) == 72) %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

median(galtonHeights_Father_72$son)


galtonHeights %>% mutate(father_strata = factor(round(father))) %>% 
                  ggplot(aes(father_strata,son)) +
                  geom_boxplot() +
                  geom_point()



galtonHeights  %>% mutate(father_strata = factor(round(father))) %>%
                  group_by(father_strata) %>%
                  summarize(cond_avg = mean(son)) %>%
                  ggplot(aes(father_strata, cond_avg)) +
                  geom_point(alpha =  0.5)


r <- galtonHeights %>% summarize(r = cor(father,son)) %>% .$r
r


GH <- galtonHeights  %>% mutate(father_strata = factor(round(father))) %>%
  group_by(father_strata) %>%
  summarize(cond_avg = mean(son))
GH

scale(as.numeric(GH$father_strata))

scale(as.numeric(GH$cond_avg))


GH %>% muta 
  ggplot(aes(as.numeric(GH$father_strata), scale(as.numeric(GH$cond_avg)))) +
      geom_point() +
      geom_abline(intercept = 0, slope = r)
      

r <- galtonHeights %>% summarize(r = cor(father,son)) %>% .$r
  
galtonHeights  %>% mutate(father_strata = round(father)) %>%
    group_by(father_strata) %>%
    summarize(cond_avg = mean(son)) %>%
    ggplot(aes(scale(father_strata), scale(cond_avg))) +
    geom_point() +
    geom_abline(intercept = 0, slope = r)


# find son's height from father's height

mu_x <- mean(galtonHeights$father)
mu_y <- mean(galtonHeights$son)
sd_x <- sd(galtonHeights$father)
sd_y <- sd(galtonHeights$son)
r <- cor(galtonHeights$father,galtonHeights$son)

m <- r * sd_y/sd_x
b <- mu_y - slope * mu_x

galtonHeights %>% ggplot(aes(father,son)) + geom_point() + geom_abline(intercept = b, slope = m)

# if sd_y and sd_x = 1, mu_x, mu_y = 0

galtonHeights %>% ggplot(aes(scale(father), scale(son))) + geom_point() + geom_abline(intercept = 0, slope = r)


galtonHeights %>% mutate(z_father = round(( father - mean(father) ) / sd(father) ), 
                         another_z_father = round(scale(father)))


# find father's height from son's height

mu_x <- mean(galtonHeights$father)
mu_y <- mean(galtonHeights$son)
sd_x <- sd(galtonHeights$father)
sd_y <- sd(galtonHeights$son)
r <- cor(galtonHeights$father,galtonHeights$son)

m <- r * sd_x/sd_y
b <- mu_x - slope * mu_y

y = 71.8

x = b + m * y
x
b
m

Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>% 
  summarize(cor(BB,HR), cor(Singles,HR), cor(Singles,BB))

# Stratifying HomeRun Per Game to Closest 10th

dat <- Teams %>% filter(yearID %in% c(1961:2001)) %>% 
                  mutate(HRPerGame = HR/G,
                         HRStrata = round(HR/G,1),
                         BBPerGame = BB/G,
                         RPerGame = R/G) %>% 
                  filter( HRStrata >= 0.4 & HRStrata <= 1.2)

head(dat)

dat %>% ggplot(aes(BBPerGame,RPerGame)) + 
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm") +
        facet_wrap(~HRStrata)

dat %>% group_by(HRStrata) %>% 
        summarize(slope = cor(BBPerGame,RPerGame) * sd(RPerGame) / sd(BBPerGame))
        
Teams %>% filter(yearID %in% c(1961:2001)) %>% 
          mutate(singlesPerGame = (H-HR-X2B-X3B)/G, rPerGame= R/G) %>% 
          summarize(slope = cor(rPerGame,singlesPerGame) * sd(rPerGame) / sd(singlesPerGame))


# Stratifying Bases on Balls to Closest 10th

dat <- Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(HRPerGame = HR/G,
         BBPerGame = BB/G,
         BBStrata = round(BB/G,1),
         RPerGame = R/G) %>% 
  filter( BBStrata >= 2.8 & BBStrata <= 3.9)

#head(dat)

dat %>% ggplot(aes(HRPerGame,RPerGame)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~BBStrata)

dat %>% group_by(BBStrata) %>% 
  summarize(slope = cor(HRPerGame,RPerGame) * sd(RPerGame) / sd(HRPerGame))


lm(son ~ father, data = galtonHeights)


galtonHeights <- galtonHeights %>%
  mutate(father_centered=father - mean(father))

lm(son ~ father_centered, data = galtonHeights)


rss <- function(b0, b1, data) {
  residue <- data$son - (b0 + b1*(data$father))
  return(sum(residue^2))
}

b1 <- seq(0,1,len = nrow(galtonHeights))
# b1
results <- data.frame(b1 = b1,
                      rss = sapply(b1, rss, b0 = 36, data = galtonHeights))

nrow(results)
head(results,n=10)


results %>% ggplot(aes(b1,rss)) + geom_line() + geom_line(aes(b1,rss), col=2)

results$b1[which(results$rss==min(results$rss))]

# Bases on Balls Vs Home Run

result <- Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(hrPerGame = HR/G, 
         bbPerGame = BB/G, 
         rPerGame = R/G)

lm(rPerGame ~ hrPerGame + bbPerGame , data = result) 


B <- 1000
N <- 50
lse <- replicate(B, {
      sample_n(galtonHeights, N, replace = TRUE) %>%
      lm(son ~ father, data = . ) %>% .$coef
})
lse <- data.frame(b0 = lse[1,], b1 = lse[2,])

lse %>% summarize(cor(b0, b1))

head(lse, n=10)


hist(lse$b0)
hist(lse$b1)


B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galtonHeights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = . ) %>% .$coef
})
lse <- data.frame(b0 = lse[1,], b1 = lse[2,])

lse %>% summarize(cor(b1, b0))

# confidence interval
galtonHeights %>% ggplot(aes(son, father)) + geom_point() +geom_smooth(method = "lm")

galtonHeights %>% mutate(Y_hat = predict(lm(son ~ father, data = .))) %>%
                  ggplot(aes(Y_hat, father)) +
                  geom_point() +
                  geom_line()


fit <- galtonHeights %>% lm(son ~ father, data = .)


Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)



model <- lm(son ~ father, data = galtonHeights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galtonHeights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galtonHeights, aes(x = father, y = son))




# Stratifying HomeRun Per Game to Closest 10th

dat <- Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(HRPerGame = HR/G,
         HRStrata = round(HR/G,1),
         BBPerGame = BB/G,
         RPerGame = R/G) %>%
  select(HRStrata, BBPerGame, RPerGame) %>%
  filter( HRStrata >= 0.4 & HRStrata <= 1.2)

head(dat)

dat %>% ggplot(aes(BBPerGame,RPerGame)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~HRStrata)

dat %>% group_by(HRStrata) %>% 
  summarize(slope = cor(BBPerGame,RPerGame) * sd(RPerGame) / sd(BBPerGame))


Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(singlesPerGame = (H-HR-X2B-X3B)/G, rPerGame= R/G) %>% 
  summarize(slope = cor(rPerGame,singlesPerGame) * sd(rPerGame) / sd(singlesPerGame))



dat %>% group_by(HRStrata) %>% head(n=10)

dat %>% group_by(HRStrata) %>% class()

class(Teams[,20])


class(as_tibble(Teams)[,20])


tibble(id=c(1,2,3), func = c(mean, median, sd))
data.frame(id=c(1,2,3), func = c(mean, median, sd))

dat 

dat <- Teams %>% filter(yearID %in% c(1961:2001)) %>% 
  mutate(HRPerGame = HR/G,
         HRStrata = round(HR/G,1),
         BBPerGame = BB/G,
         RPerGame = R/G) %>%
  select(HRStrata, BBPerGame, RPerGame) %>%
  filter( HRStrata >= 0.4 & HRStrata <= 1.2)

dat %>% group_by(HRStrata) %>%
        do(fit = lm(RPerGame ~ BBPerGame, data = .))


get_slope <- function(data) {
  fit <- lm(RPerGame ~ BBPerGame, data = data)
  data.frame(slope = fit$coefficients[2],
            se = summary(fit)$coefficient[2,2])
}

dat %>% group_by(HRStrata) %>%
  do(get_slope(.))

get_lse<- function(data) {
  fit <- lm(RPerGame ~ BBPerGame, data = data)
  data.frame(terms = names(fit$coefficients),
             value = fit$coefficients,
             se = summary(fit)$coefficient[2,2])
}

dat %>% group_by(HRStrata) %>%
  do(get_lse(.))

#install.packages("broom")
library(broom)
fit <- lm(RPerGame ~ BBPerGame, data = dat)
tidy(fit)
tidy(fit, conf.int = TRUE)

library(dplyr)
library(ggplot2)
dat %>% group_by(HRStrata) %>%
  do(tidy(lm(RPerGame ~ BBPerGame, data = .), conf.int = TRUE)) %>%
  filter(term == 'BBPerGame') %>%
  select(HRStrata, estimate, conf.low, conf.high) %>%
  ggplot(aes(HRStrata, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point() 
  


# Liear Model for Base Ball
fit <- Teams %>% filter(yearID %in% 1961:2014) %>% 
         mutate(BB = BB/G,                
                singles = (H-X2B-X3B-HR)/G,
                doubles = X2B/G,
                triples = X3B/G,
                HR = HR/G,
                R = R/G) %>%
        select(BB, singles, doubles, triples, HR, R) %>%
        lm(R ~ HR + BB + R + singles + doubles + triples, data = .)

fit

coefs <- tidy(fit, conf.int = TRUE)
coefs


Teams %>% filter(yearID %in% 2015) %>% 
  mutate(BB = BB/G,                
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  select(BB, singles, doubles, triples, HR, R) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat,R)) + geom_point() + geom_abline()

library(Lahman)
data("Batting")
pa_per_game <- Batting %>% 
                filter(yearID == 2002) %>%
                group_by(teamID) %>%
                summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
                .$pa_per_game %>% mean

# run of the team if only this player for that team every time.

players <- Batting %>% 
            filter(yearID %in% 1999:2001) %>%
            group_by(playerID) %>%
            mutate(PA = BB + AB) %>%
            summarize(G = sum(PA)/pa_per_game,
                      BB = sum(BB)/G,                
                      singles = sum(H-X2B-X3B-HR)/G,
                      doubles = sum(X2B)/G,
                      triples = sum(X3B)/G,
                      HR = sum(HR)/G,
                      AVG = sum(H)/sum(AB),
                      PA = sum(PA)) %>%
            filter (PA >= 300) %>%
            select(-G) %>%
            mutate(R_hat = predict(fit, newdata = .))

hist(players$R_hat, 20)


players %>% ggplot(aes(R_hat)) +
            geom_histogram(binwidth = 0.5, color = "black")

# getting players salary during 2002
players <- Salaries %>%
            filter(yearID == 2002) %>%
            select(playerID, salary) %>%
            right_join(players, by = "playerID")

# getting fielding positon. position that players took most number of times.
players <- Fielding %>% filter(yearID == 2002) %>% 
            filter(!POS %in% c("OF", "P")) %>%
            group_by(playerID) %>%
            top_n(1, G) %>%
            filter(row_number(G) == 1) %>%
            ungroup() %>%
            select(playerID, POS) %>%
            right_join(players, by="playerID") %>%
            filter(!is.na(POS) & !is.na(salary))

# adding their name and last name

players <- Master %>%
            select(playerID, nameFirst, nameLast, debut) %>%
            right_join(players, by="playerID")

# filter only needed columns
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
            arrange(desc(R_hat)) %>%
            top_n(10)

# graph on R_hat metric vs salary
players %>% ggplot(aes(salary, R_hat, color = POS)) + geom_point() + scale_x_log10()

# excluding young players . do filtering players debuted before 1997

players %>% filter(debut < 1998) %>% 
            ggplot(aes(salary, R_hat, color = POS)) + 
            geom_point() + 
            scale_x_log10()

# Galileo case study. Measurement Error Models
library(dslabs)
data("falling_object")
data()

library(dplyr)
library(ggplot2)
falling_object <- rfalling_object(n=25)


falling_object %>% ggplot(aes(time, observed_distance)) + geom_point()



fit <- falling_object %>% 
  mutate(time_sq = time^2) %>%
  lm(observed_distance ~ time + time_sq, data = .)

library(tidyverse)
library(broom)
tidy(fit)

augment(fit) %>% ggplot() + 
                  geom_point(aes(time, observed_distance)) + 
                  geom_line(aes(time, .fitted)) 


# The Tower of Pisa height is within the confidence interval for beta 0.
# The initial velocity of 0 is in the confidence interval for beta 1.
# 
# term        estimate std.error statistic  p.value conf.low conf.high
# <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
# 1 (Intercept)   56.5       0.508   111.    9.30e-32    55.4     57.5  
# 2 time          -0.613     0.723    -0.848 4.06e- 1    -2.11     0.887
# 3 time_sq       -4.77      0.215   -22.2   1.50e-16    -5.22    -4.33 

tidy(fit, conf.int = TRUE)

# Correlation is Not Causation: Spurious Correlation
# data dredging,  data phishing, data snooping or (In United States) cherry picking.

# Variation called - p-hacking

N <- 25
G <- 1000000
simulation_data <- tibble(group = rep(1:G, N), X = rnorm(N*G), Y=rnorm(N*G))  

res <- simulation_data %>% 
        group_by(group) %>%
        summarize(r = cor(X,Y))
res %>% arrange(desc(r))


simulation_data %>% filter(group == res$group[which.max(res$r)]) %>%
                    ggplot(aes(X,Y)) +
                    geom_point() +
                    geom_smooth(method = "lm")

hist(res$r)


simulation_data %>% filter(group == res$group[which.max(res$r)]) %>%
                    do(tidy(lm(X ~ Y, data = .)))


# Outliers

set.seed(1)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

tibble(x,y) %>% ggplot(aes(x,y)) + geom_point(alpha = 0.5)
plot(X = x,y = y)

cor(x[-23],y[-23])


tibble(x[-23],y[-23]) %>% ggplot(aes(x[-23],y[-23])) + geom_point(alpha = 0.5)

tibble(rank(x),rank(y)) %>% ggplot(aes(rank(x),rank(y))) + geom_point(alpha = 0.5)


cor(rank(x),rank(y))


# Correlation is Not Causation: Reversing Cause and Effect

# Correlation is not Causation: Confounders

data("admissions")
admissions %>% group_by(gender) %>% summarize(per = sum(applicants*admitted)/sum(applicants))

admissions %>% group_by(gender) %>%
              summarize(totalAdmitted = round(sum(admitted/100*applicants)),
                        notAdmitted = sum(applicants)- sum(totalAdmitted)) %>%
                        select(-gender) %>%
                        do(tidy(chisq.test(.)))

admissions %>% ggplot(aes(major, admitted, col= gender, size=applicants)) + geom_point()
