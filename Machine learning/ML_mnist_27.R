library(tidyverse)
library(caret)
library(dslabs)

mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1),        
                             which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2),        
                             which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()


mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

#======exercice loess=====
library(dslabs)
library(broom)
data("mnist_27")
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

#try to solve problem
fit<- loess(as.numeric(y) ~ x_2,mnist_27[["train"]])
as.data.frame(mnist_27$train) %>% mutate(smooth=predict(fit,x_2))%>%
  ggplot(aes(x_2,y))+geom_point()+geom_line(aes(x_2,smooth),col="red")

#solution
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")

#=======Matrix exercices====
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000,]

#see the 3d row which is an image of 4:
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)
image(1:28, 1:28, grid[, 28:1])
avg <- rowMeans(x)
tibble(labels = as.factor(y), row_averages = avg) %>% 
  qplot(labels, row_averages, data = ., geom = "boxplot") 

#detect inchanging columns
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

#remove non informative pixels
new_x <- x[ ,colSds(x) > 60]
dim(new_x)

#grey zone
x <- mnist$train$images
y <- mnist$train$labels

xg <- rowMeans((x>=50 & x<=205))
qplot(as.factor(y),xg,geom="boxplot")

mean(x>=50 & x<=205) #proportion of grey zone

#=======bootstrap====
library(dslabs)
data("mnist_27")
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

times<- sapply(1:10,function(i) sum(indexes[[i]]==7))
sum(times)

y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y,0.75)
#monte carlo simulation
B<- 10^4
set.seed(1)
q75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y,0.75)})
mean(q75)

#use 10 bootstrap
set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
ind <- createResample(y,10)

me <- sapply(1:10,function(i) quantile(y[ind[[i]]],0.75))
mean(me)
sd(me)

#solution given
set.seed(1)
indexes <- createResample(y, 10^4)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

