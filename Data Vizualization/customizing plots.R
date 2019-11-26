library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)
dat$state<-reorder(dat$state,dat$rate)#reorder levels of state from Alphabetical to highest rate on top
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

#*****
library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
murders %>% mutate(rate = total/population*100000) %>%
  mutate(region=reorder(region, rate, FUN=median)) %>%
  ggplot(aes(region, rate)) +
  geom_boxplot() +
  geom_point()