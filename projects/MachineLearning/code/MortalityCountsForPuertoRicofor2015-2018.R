library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)


fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

dat

totalDays <- diff(range(dat$date))

totalDays


head(dat$date)
tail(dat$date)

which(dat$date=='2015-03-01')

span <- 60 / as.numeric(diff(range(dat$date)))

span

span <- 238/1216

span

fit_loess <- loess(deaths ~ date, degree = 1, span = span, data = dat)

span <- 60 / as.numeric(diff(range(dat$date)))

span

dat %>% ggplot(aes(date, deaths)) +
  geom_point() + 
  geom_smooth(color="red", span = span, method.args = list(degree=1))


as.numeric(dat$date)


span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)

fit$fitted

dat  %>% mutate(smooth_1 = fit$fitted) %>% 
  ggplot(aes(x,deaths)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day,smooth_1), color = "red") +
  geom_line(aes(day,smooth_2), color = "blue") 



dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")


dat %>% 
  mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = mday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth)) +
  geom_line(lwd = 2)


dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train)




qplot(x_2, y, data = mnist_27$train)



mnist_27$train %>% ggplot(aes(x_2, y)) +
  geom_point() + 
  geom_smooth()


fit_loess <- loess(y ~ x_2, degree = 1, span = 0.1, data = mnist_27$train)

mnist_27$train %>% mutate(smooth_fit = fit_loess$fitted) %>% 
  ggplot(aes(x_2,y)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(x_2,smooth_fit), color = "red")


dat %>% 
  mutate(smooth = predict(fit_loess, as.numeric(x_2)), y = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_point() +
  geom_smooth(method = "loess")




library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

str(dat)

nrow(dat)

max(dat$day)

span <- 60/nrow(dat)

fit_loess <- loess(deaths ~ date, degree = 1, span = span, data = dat)

nrow(fit_loess)

dat %>% mutate(smooth = fit_loess$fitted) %>% 
  ggplot(aes(deaths,date)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day,smooth), color = "red") +
  geom_line(aes(day,smooth_2), color = "blue") 
