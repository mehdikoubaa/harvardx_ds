library(tidyverse)
library(caret)

set.seed(2) #if you are using R 3.5 or earlier
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
dat$train %>% ggplot(aes(x, color = y)) + geom_density()

#_______________________
set.seed(1)
mu_1 <- seq(0, 3, len=25)
make_data2 <- function(mu_1, n = 1000, p = 0.5, 
                      mu_0 = 0, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

v <-sapply(mu_1,make_data2,simplify = F)

f <-sapply(seq(1,25,1),function(i) {
  glmfit<-glm(v[[i]][["train"]]$y ~ v[[i]][["train"]]$x, data=v[[i]][["train"]],family = "binomial")
  p_hat<-predict(glmfit,v[[i]][["test"]],type = "response")
  y_hat <- if_else(p_hat>0.5,1,0)%>% factor()
  v[[i]][["test"]]$y <- as.factor(v[[i]][["test"]]$y)
  confusionMatrix(y_hat, v[[i]][["test"]]$y)$overall[["Accuracy"]]
})

dd <- data.frame(mu_1,f)
ggplot(dd,aes(mu_1, f))+geom_point()

#______solutoin given

set.seed(1) #if you are using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)
