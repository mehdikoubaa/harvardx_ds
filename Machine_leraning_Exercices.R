
#install.packages('caret', dependencies = TRUE)

library(dslabs)
library(dplyr)
library(lubridate)
library(caret)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

y_hat<- if_else(x=="inclass","Female","Male") %>% factor(levels = levels(y))

mean(y_hat==y)

table(y_hat,y)

sensitivity(y_hat, y)

specificity(y_hat, y)

confusionMatrix(data = y_hat, reference = y)

#______________Iris
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
factor(train$Species,levels=levels(train$Species))


cutoff <- function(x) {
 ind <- which(train$Species=="virginica")
 mean(x[ind])-sd(x[ind])
}

y_hat<- matrix(nrow=nrow(train),ncol = 4)
for (i in 1:4) {
  y_hat[,i] <- if_else(train[,i]>=cutoff(train[,i]),"virginica","versicolor")
print(mean(train$Species==y_hat[,i]))
}

range(train[,1])
