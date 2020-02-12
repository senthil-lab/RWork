#install.packages("swirl")

library(swirl)

install_course_zip("C:/Users/snara016/Downloads/14_310x_Intro_to_R.zip",multi=FALSE)


# 
# | You can exit swirl and return to the R prompt (>) at any time by pressing the Esc key. If you
# | are already at the prompt, type bye() to exit and save your progress. When you exit properly,
# | you'll see a short message letting you know you've done so.
# 
# | When you are at the R prompt (>):
#   | -- Typing skip() allows you to skip the current question.
# | -- Typing play() lets you experiment with R on your own; swirl will ignore what you do...
# | -- UNTIL you type nxt() which will regain swirl's attention.
# | -- Typing bye() causes swirl to exit. Your progress will be saved.
# | -- Typing main() returns you to swirl's main menu.
# | -- Typing info() displays these options again.

library(tidyverse)
papers <- as_tibble(read_csv("CitesforSara.csv"))
names(papers)
papers_select <- select(papers, journal, year, cites, title, au1)
nrow(filter(papers_select, cites >= 100))
papers_select %>% group_by(journal) %>% summarize(sum(cites))

length(unique(papers_select$au1))

papers_female <- select(papers, contains("female"))
head(papers_female)


install.packages("tidyverse")