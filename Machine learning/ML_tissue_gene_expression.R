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
