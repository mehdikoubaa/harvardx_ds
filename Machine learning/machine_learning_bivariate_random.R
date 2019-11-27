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

#•_____________________an ohter example_______________________________ 
#3.1: Linear Regression for Prediction  Comprehension Check: Linear Regression

set.seed(1) # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
v <- replicate(n,{
  index_t <- createDataPartition(dat$y,times=1,p=0.5,list=F)
  train_set<-dat %>% slice(-index_t)
  test_set <- dat %>% slice(index_t)
  fit <- lm(train_set$y ~train_set$x,data=train_set)
  y_hat<- fit$coef[1]+ fit$coef[2]*test_set$x
  RMSE(y_hat,test_set$y)
})
c(mean(v), sd(v))


#____________ n is vecotr now
n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
vect <- function(n){
  set.seed(1)
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  set.seed(1)
  v <- replicate(n,{
  index_t <- createDataPartition(dat$y,times=1,p=0.5,list=F)
  train_set<-dat %>% slice(-index_t)
  test_set <- dat %>% slice(index_t)
  fit <- lm(train_set$y ~train_set$x,data=train_set)
  y_hat<- fit$coef[1]+ fit$coef[2]*test_set$x
  RMSE(y_hat,test_set$y)
})
  c(mean(v), sd(v))
}
n <- c(100, 500, 1000, 5000, 10000)
vvect <- sapply(n,vect)

#_____change in dat matrix
set.seed(1)
n2 <- 100
Sigma2 <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat2 <- MASS::mvrnorm(n= 100, c(69, 69), Sigma2) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1)
v2 <- replicate(n2,{
  index_t <- createDataPartition(dat2$y,times=1,p=0.5,list=F)
  train_set<-dat2 %>% slice(-index_t)
  test_set <- dat2 %>% slice(index_t)
  fit <- lm(train_set$y ~train_set$x,data=train_set)
  y_hat<- fit$coef[1]+ fit$coef[2]*test_set$x
  RMSE(y_hat,test_set$y)
})
c(mean(v2), sd(v2))

#_____________matrix [3*3]
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1)
ind <- createDataPartition(dat$y,times=1,p=0.5,list=F)
train_set <- dat[-ind,]
test_set <- dat[ind,]
fit <- train_set %>% lm(y ~ x_1 + x_2,.)
fit1 <- train_set %>% lm(y ~ x_1,.)
fit2 <- train_set %>% lm(y ~ x_2,.)

y_hat <- fit$coef[1]+ fit$coef[2]*test_set$x_1+ fit$coef[3]*test_set$x_2
y_hat1 <- predict(fit1, test_set)
y_hat2 <- predict(fit2, test_set)

RMSE(y_hat,test_set$y )
RMSE(y_hat1,test_set$y )
RMSE(y_hat2,test_set$y )

#_____________matrix [3*3] x1 and x2 are highly correlated
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1)
ind <- createDataPartition(dat$y,times=1,p=0.5,list=F)
train_set <- dat[-ind,]
test_set <- dat[ind,]
fit <- train_set %>% lm(y ~ x_1 + x_2,.)
fit1 <- train_set %>% lm(y ~ x_1,.)
fit2 <- train_set %>% lm(y ~ x_2,.)

y_hat <- fit$coef[1]+ fit$coef[2]*test_set$x_1+ fit$coef[3]*test_set$x_2
y_hat1 <- predict(fit1, test_set)
y_hat2 <- predict(fit2, test_set)

RMSE(y_hat,test_set$y )
RMSE(y_hat1,test_set$y )
RMSE(y_hat2,test_set$y )
