library(tidyverse)
library(caret)
library(dslabs)
data("mnist_27")

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

set.seed(1, sample.kind = "Rounding")# in R 3.6 or later

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

y_hats <- sapply(fits, function(object) {
                          predict (object, mnist_27$test)
                                          })

#accuracy for each y_hat
accuracies <- sapply(1:10, function(i) {
  mean(y_hats[,i]==mnist_27$test$y)
})
names(accuracies) <- colnames(y_hats)
accuracies
mean(accuracies)


# #build an ensemble prediction by majority vote
# and compute the accuracy of the ensemble.

y_hat_ensemble <-sapply(1:nrow(y_hats), function (i) {
  
  if_else(sum(y_hats[i,]==2)/ncol(y_hats)>0.5 ,2 , 7) 
          })
#more simple
y_hat_ensemble <- if_else(rowMeans(y_hats==2)>0.5,2,7 )

mean(y_hat_ensemble==mnist_27$test$y)

#accuracy estimates obtained from cross validation with the training data
test_accuracies <- sapply(1:10,function(i) {
  
       min( fits[[i]]$results[["Accuracy"]])
  } )
names(test_accuracies) <- colnames(y_hats)
test_accuracies
mean(test_accuracies)

# choose to set only accuracies of test over than 0.8
ind <- test_accuracies>=0.8

#accuracy of the ensemble (by vote) for selected methods which test acc. >= 0.8

y_hat_ensemble2 <- if_else(rowMeans(y_hats[,ind]==2)>0.5,2,7 )

mean(y_hat_ensemble2==mnist_27$test$y)

