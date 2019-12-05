library(tidyverse)
library(rpart)
n <- 1000
sigma <- 0.25
#set.seed(1) 
set.seed(1, sample.kind = "Rounding") #if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)

plot(fit)
text(fit,cex=0.5)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

library(randomForest)
fit2 <-  randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
plot(fit2)


#It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth).
#Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.
fit3 <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit3)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  