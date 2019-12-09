library(tidyverse)
library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123,sample.kind = "Rounding")
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
#note that the line above is the corrected code - code in video at 0:52 is incorrect
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

#Preprocessing MNIST Data
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256)

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

#The caret package requires that we add column names to the feature matrices.
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

#============= knn model =====================

#In general, it is a good idea to test out a small subset of the data first 
#to get an idea of how long your code will take to run.


control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[ ,col_index], y, 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
train_knn

#it is a good idea to try a test run with a subset of the data to get an idea of
# timing before we start running code that might take hours to complete
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index, col_index], y[index], 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

#Once we optimize our algorithm, we can fit it to the entire dataset:
  fit_knn <- knn3(x[, col_index], y,  k = 3)
  
  y_hat_knn <- predict(fit_knn, x_test[, col_index], type="class")
  cm <- confusionMatrix(y_hat_knn, factor(y_test))
  cm$overall["Accuracy"] #=0.953
  
#============ Random Forest Model ==========
  
#Because with random forest the fitting is the slowest part of the procedure 
#rather than the predicting (as with kNN),we will use only five-fold cross validation
#We will also reduce the number of trees that are fit since we are not yet building our final model.
  library(randomForest)
  control <- trainControl(method="cv", number = 5)
  grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
  
  train_rf <-  train(x[, col_index], y, 
                     method = "rf", 
                     ntree = 150,
                     trControl = control,
                     tuneGrid = grid,
                     nSamp = 5000)
  
  ggplot(train_rf)
  train_rf$bestTune #mtry >  10
  
  fit_rf <- randomForest(x[, col_index], y, 
                         minNode = train_rf$bestTune$mtry)
  #To check that we ran enough trees we can use the plot function:
    
    plot(fit_rf)
  #We see that we achieve high accuracy:
    
    y_hat_rf <- predict(fit_rf, x_test[ ,col_index])
  cm <- confusionMatrix(y_hat_rf, y_test)
  cm$overall["Accuracy"]
  #> Accuracy 
  #>    0.952
  
#Rborist model in random forest: more rapid and less tuning features  
  library(Rborist)
  control <- trainControl(method="cv", number = 5, p = 0.8)
  grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
  train_rf <-  train(x[, col_index], y,
                     method = "Rborist",
                     nTree = 50,
                     trControl = control,
                     tuneGrid = grid,
                     nSamp = 5000)
  ggplot(train_rf)
  train_rf$bestTune
  
  fit_rf <- Rborist(x[, col_index], y,
                    nTree = 1000,
                    minNode = train_rf$bestTune$minNode,
                    predFixed = train_rf$bestTune$predFixed)
  
  y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
  cm <- confusionMatrix(y_hat_rf, y_test)
  cm$overall["Accuracy"]

  #check some examples  
  rafalib::mypar(3,4)
  for(i in 1:12){
    image(matrix(x_test[i,], 28, 28)[, 28:1], 
          main = paste("Our prediction:", y_hat_rf[i]),
          xaxt="n", yaxt="n")
  } 
  
# Rborist package does not currently support variable importance calculations,
# but the randomForest package does
  library(randomForest)
  x <- mnist$train$images[index,]
  y <- factor(mnist$train$labels[index])
  rf <- randomForest(x, y,  ntree = 50)
  imp <- importance(rf)
  imp
  
  image(matrix(imp, 28, 28))
  
# ==========visualizing results to determine why we are failing ======
  
# KNN
  p_max <- predict(fit_knn, x_test[,col_index])
  p_max <- apply(p_max, 1, max)
  ind  <- which(y_hat_knn != y_test)
  ind <- ind[order(p_max[ind], decreasing = TRUE)]
  rafalib::mypar(3,4)
  for(i in ind[1:12]){
    image(matrix(x_test[i,], 28, 28)[, 28:1],
          main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                        " but is a ",y_test[i]),
          xaxt="n", yaxt="n")
  }
# Random Forest  
  p_max <- predict(fit_rf, x_test[,col_index])$census  
  p_max <- p_max / rowSums(p_max)
  p_max <- apply(p_max, 1, max)
  ind  <- which(y_hat_rf != y_test)
  ind <- ind[order(p_max[ind], decreasing = TRUE)]
  rafalib::mypar(3,4)
  for(i in ind[1:12]){
    image(matrix(x_test[i,], 28, 28)[, 28:1], 
          main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                        " but is a ",y_test[i]),
          xaxt="n", yaxt="n")
  }
  

#========= Ensembles ========
  # ombine multiple machine learning algorithms into one model to improve predictions.
  p_rf <- predict(fit_rf, x_test[,col_index])$census
  p_rf <- p_rf / rowSums(p_rf)
  p_knn <- predict(fit_knn, x_test[,col_index])
  p <- (p_rf + p_knn)/2
  y_pred <- factor(apply(p, 1, which.max)-1)
  confusionMatrix(y_pred, y_test)