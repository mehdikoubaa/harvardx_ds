library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
d <- dist(tissue_gene_expression$x)


f <- as.matrix(d)[c(1,2,39,40,73,74),c(1,2,39,40,73,74)]


image(as.matrix(d))

#____exercice but dosent work for me
library(caret)
set.seed(1)
ind <- createDataPartition(tissue_gene_expression$y,times = 1,p=0.5,list=F)

df <- cbind(y=tissue_gene_expression$y,tissue_gene_expression$x)
train_set <- df[-ind,]
test_set <- df[ind,]
k <- seq(1,11,2)
accuracy <- sapply(k, function(i) {
  
  fit <- knn3(y ~ .,data = train_set,k=i)
  y_hat <- predict(fit,test_set,type="class")
  confusionMatrix(y_hat,test_h$sex)$overall[["Accuracy"]]
  
})

#solution given_______
set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})

#train fonction with knn
fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
fit$results


#__________Generative models
#Use the train function to estimate the accuracy of LDA. For this question, use the entire tissue_gene_expression dataset: 
#do not split it into training and test sets (understand this can lead to overfitting).
library(tidyverse)
library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993) #set.seed(1993, sample.kind="Rounding") #if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
fit_lda <- train(x,y,method="lda")


#solution
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#QDA
set.seed(1993) #set.seed(1993, sample.kind="Rounding") #if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
fit_qda <- train(x,y,method="qda")
fit_qda$results$Accuracy

t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

fit_lda <- train(x, y, method = "lda", preProcess = c("center"))
fit_lda$results["Accuracy"]
       