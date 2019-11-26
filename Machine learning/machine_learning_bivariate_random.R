library(tidyverse)
library(MASS)
library(caret)


Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>% 
  qplot(x, y, data =.)

#•_____an ohter example 
#3.1: Linear Regression for Prediction  Comprehension Check: Linear Regression
set.seed(1) # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
v <- replicate(n,{
  index_t <- createDataPartition(dat$y,times=1,p=0.5,list=F)
  train_set<-dat %>% slice(-index_t)
  test_set <- dat %>% slice(index_t)
  fit <- lm(train_set$y ~train_set$x,data=train_set)
  y_hat<- predict(fit,test_set)
  RMSE(y_hat,test_set$y)
})

mean(v)
sd(v)
