library(tidyverse)


murders %>% mutate(abb = reorder(abb, rate)) %>%
            ggplot(aes(abb, rate)) +
            geom_bar(width = 0.5, stat = "identity", color = "black") +
            coord_flip()

ggsave("projects/murders/figs/barplot.png")