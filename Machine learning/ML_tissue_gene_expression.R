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
set.seed(1,sample.kind = "Rounding")
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

#_________Comprehension Check: Caret Package >> exercices
#Q1
library(tidyverse)
library(caret)
library(dslabs)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
set.seed(1991,sample.kind = "Rounding")
fit_rpart <- train(x,y,method="rpart",tuneGrid = data.frame(cp=seq(0, 0.1, 0.01)))
plot(fit)

#Q2
set.seed(1991,sample.kind = "Rounding")
fit_rpart <- train(x,y,
             method="rpart",
             tuneGrid = data.frame(cp=seq(0, 0.1, 0.01)),
             control = rpart.control(minsplit = 0))
ggplot(fit)
confusionMatrix(fit)

#Q3
df <- data.frame(x=x,y=y)
set.seed(1991,sample.kind = "Rounding")
fit_r <- rpart(y~x,data=df,cp=0)
plot(fit)
text(fit_r,size=0.25,pos=1,cex=0.5)
#solution given
plot(fit$finalModel)
text(fit$finalModel)

#Q4
library(randomForest)
set.seed(1991,sample.kind = "Rounding")
fit <- train(x,y,
             method="rf",
             tuneGrid = data.frame( mtry=seq(50, 200, 25)) , 
             nodesize=1 )
plot(fit)

#Q5 Use the function varImp on the output of train and save it to an object called imp
imp <- varImp(fit)
imp

#Q6
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

imp$importance["CFHR4",1]

tmp <- imp$importance %>% arrange(desc(Overall))
which(tmp==imp$importance["CFHR4",1])

#solution_given
data_frame(term = row.names(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)