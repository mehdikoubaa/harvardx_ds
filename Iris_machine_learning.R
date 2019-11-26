library(tidyverse)
library(caret)

data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]


#___________________Solution proposed
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	

#_______test solution

predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs,'virginica','versicolor')
mean(y_hat==test[,5]) # accuracy ->0.9

plot(iris,pch=21,bg=iris$Species)

#_____adding new fetaure for more accuracy: Petal.Width

pred.L <- foo(train[,3])
rg.val.L <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoff.L <-rg.val.L[which(pred.L==max(pred.L))]

pred.W <- foo(train[,4])
rg.val.W <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoff.W <-rg.val.W[which(pred.W==max(pred.W))]

y_hat <- ifelse(test[,3]>cutoff.L | test[,4]>cutoff.W,'virginica','versicolor')
mean(y_hat==test[,5]) # accuracy ->0.9

#_____________overall solution

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)


