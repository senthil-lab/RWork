# getwd()
setwd("C:/Users/snara016/OneDrive - Ascension/Documents/RWork/projects/Wrangling/data")
system.file("extdata",package = "dslabs")
list.files("C:/Users/snara016/OneDrive - Ascension/Documents/R/win-library/3.6/dslabs/extdata")

path <- "C:/Users/snara016/OneDrive - Ascension/Documents/R/win-library/3.6/dslabs/extdata"
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath


file.copy(fullpath,getwd())

file.exists(filename)

library(readr)
read_lines("murders.csv", n_max = 3)

dat <- read_csv(filename)
class(dat)


class(dat$state)

dat2 <- read.csv(filename)
class(dat2$state)

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat3 <- read_csv(url)
class(dat3)

download.file(url,"murders1.csv")


tempfile()
tempdir()


tmp_file <- tempfile()
download.file(url,tmp_file)
dat4 <- read_csv(tmp_file)
file.remove(tmp_file)


class(dat4)


path <- system.file("extdata",package = "dslabs")
list.files(path)


filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

class(wide_data)

library(tidyr)

new_tidy_data <- wide_data %>% gather(year, fertility, '1960', '1961':'2015',convert = TRUE)
new_tidy_data

head(wide_data)

new_tidy_data <- wide_data %>% gather(year, fertility, -country)
new_tidy_data


new_tidy_data <- wide_data %>% gather(year, fertility, -country, convert = TRUE)
new_tidy_data


new_wide_data <- new_tidy_data %>% spread(year, fertility) 
head(new_wide_data)


path <- system.file("extdata",package = "dslabs")
list.files(path)

filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
wide_data_1 <- read_csv(filename)

head(wide_data_1)

new_tidy_data_1 <- wide_data_1 %>% gather(key , val, -country)
new_tidy_data_1

new_tidy_data_1 <- new_tidy_data_1 %>% separate(key, c("year","var_1","var_2"), "_")
new_tidy_data_1



new_tidy_data_1 <- wide_data_1 %>% gather(key , val, -country)
new_tidy_data_1

new_tidy_data_1 <- new_tidy_data_1 %>% separate(key, c("year","var_1"), sep = "_", extra = "merge")
new_tidy_data_1


new_tidy_data_2 <- new_tidy_data_1 %>% spread(var_1,val)
new_tidy_data_2


# combining tables
library(dplyr)
library(dslabs)
data("murders")
data("polls_us_election_2016")
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

library(ggplot2)
library(ggrepel)
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
        geom_point() +
        geom_text_repel() +
        scale_x_continuous(trans = "log2") +
        scale_y_continuous(trans = "log2") +
        geom_smooth(method = "lm", se = FALSE)
        

# variation of join
tab1 <- slice(murders, 1:6)
head(tab1)

tab1

tab2 <- slice(results_us_election_2016, c(1:3,5,7,8))
tab2
tab <- left_join(tab1, tab2, by = "state")
tab

tab <- right_join(tab1, tab2, by = "state")
tab

left_join(tab1, tab2)
right_join(tab1, tab2)
inner_join(tab1,tab2)
full_join(tab1,tab2)


semi_join(tab1,tab2)
anti_join(tab1,tab2)

# dirty joins - bindings

tab1 <- tab[, 1:3]
tab1

tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1,tab2,tab2)
new_tab_c <- cbind(tab1,tab2,tab2)
class(new_tab)

# set operators

intersect(1:10, 9:20)
# 9 10

intersect(c('a','b','c'), c('b','d','e'))
# "b"

union(1:10, 9:20)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

setdiff(1:10, 9:20)
# 1 2 3 4 5 6 7 8

setdiff(9:20, 1:10)
# 11 12 13 14 15 16 17 18 19 20

setequal(1:5,1:6)
# FALSE

setequal(1:5,5:1)
# TRUE

# Web Scarping

library(rvest)
url <- "https://courses.edx.org/dashboard"
h <- read_html(url)

class(h)

head(h)

h


# string Processing

parse_numbers

s <- '10"'
s
cat(s)

s <- "5\'10\""
cat(s)

library(dslabs)
data("reported_heights")

class(reported_heights$height)


x<-as.numeric(reported_heights$height)
length(is.na(x) == TRUE)
sum(is.na(x))


library(dplyr)
library(tidyverse)
reported_heights %>% filter(is.na(as.numeric(height)) == TRUE) %>% head()

not_inch <- function(x){
        inches = as.numeric(x)
        ind <- is.na(inches) | inches < 50 | inches > 84
        ind
}

problems <- reported_heights %>% filter(not_inch(height)) %>% .$height


length(problems)


pattern_1 <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*\"*$"
str_subset(problems,pattern_1) %>% head(n=10)



pattern_2 <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems,pattern_2) %>% head(n=10) %>% cat


ind <- which(between(suppressWarnings(as.numeric(problems))/2.54,54,81))
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat


pattern_comma <- ","
ind <- str_detect(reported_heights$height, pattern_comma) 


pattern_cm <- "cm"
str_subset(reported_heights$height, pattern_cm) 


yes <- c("180 cm", "70 inches")
no <- c("180","70''")
s <- c(yes,no)
s

str_subset(s, "cm")
str_subset(s, "inches")

str_detect(s, "cm") | str_detect(s, "inches")

str_detect(s, "cm|inches")



yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("",".","Five", "six")
s <- c(yes,no)
s

pattern_d <- "\\d"
str_detect(s, pattern_d)
# install.packages("htmlwidgets")
library(htmlwidgets)
str_view(s, pattern_d)

str_view_all(s, pattern_d)


# character class


str_view_all(s, "[56]")

str_view_all(s, "[4-9]|10|11")


# Anchor ^ - start $ - end
str_view(s, "^\\d$")

# {1,2} How many digits?

str_view(s, "^\\d'\\d{1,2}$")

ind <-which(str_detect(reported_heights$height, "^[4-7]'\\d{1,2}\"$") == TRUE)
length(ind)

cat(reported_heights$height[ind])

str_view(s, "^\\d'\\d{1,2}$")

new_pattern <- "^[4-7]'\\d{1,2}$"
problems %>% str_replace("feet|ft|foot", "'") %>% 
                str_replace("inches|in|''|\"", "") %>%
                str_detect(new_pattern) %>% sum


new_pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems,new_pattern_2)

pattern_3 <- "^[4-7]\\s*'\\s*\\d{1,2}\"$"
str_subset(problems,pattern_3)

new_pattern_3 <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% str_replace("feet|ft|foot", "'") %>% 
        str_replace("inches|in|''|\"", "") %>%
        str_detect(new_pattern_3) %>% sum


patterns_without_groups <- "^[4-7],\\d*$"
patterns_with_groups <- "^([4-7]),(\\d*)$"


str_match(problems, patterns_with_groups)
str_extract(problems, patterns_with_groups)


# \\i - value of ith group

patterns_with_groups <- "^([4-7]),(\\d*)$"
yes <- c("5,9","5,11","6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, patterns_with_groups, "\\1'\\2")


new_pattern_3 <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
str_subset(problems, new_pattern_3) %>% str_replace(new_pattern_3, "\\1'\\2") %>% cat


not_inches_or_cm <- function(x, smallest = 50, tallest = 84) {
        inches <- suppressWarnings(as.numeric(x))
        ind <- !is.na(inches) &
                ((inches >= smallest & inches <= tallest) |
                 (inches/2.54 >= smallest & inches/2.54 <= tallest))
        !ind
}



new_problems <- reported_heights %>% filter(not_inches_or_cm(height)) %>% 
                .$height

head(new_problems, n=500)

length(new_problems)
length(reported_heights$height)

converted <- new_problems %>%
        str_replace("feet|ft|foot", "'") %>% 
        str_replace("inches|in|''|\"", "") %>%
        str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")


pattern_4 <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern_4)
mean(index)

converted[!index]


# seperate with regex

s <- c("5'10", "6'1")
tab <- data.frame(x = s)

library(tidyr)
tab
tab %>% separate(x, c("feet", "inches"), sep ="'")

tab %>% extract(x, c("feet", "inches"), regex ="(\\d)'(\\d{1,2})")


# string splitting
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
head(lines)

x <- str_split(lines, ",")
head(x)

col_names <- x[[1]]
x <- x[-1]


library(purrr)
map(x, function(y) y[1]) %>% head()

map(x, 1) %>% head()

library(readr)

dat <- data.frame(map_chr(x,1),
                map_chr(x,2),
                map_chr(x,3),
                map_chr(x,4),
                map_chr(x,5)) %>%
                mutate_all(parse_guess) %>%
#        setNames(col_names)
head(dat)


#recode function
library(dslabs)
data("gapminder")
