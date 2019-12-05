library(tidyverse)
library(caret)
library(rpart)
library(titanic)    # loads titanic_train data frame

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

#SibSp: Number of Siblings/Spouses Aboard
#Parch: Number of Parents/Children Aboard
#Embarked: Port of Embarkation

set.seed(42, sample.kind = "Rounding")
ind <- createDataPartition(titanic_clean$Survived,times=1,list=F,p=0.2)
train_set <- titanic_clean[-ind,]
test_set <- titanic_clean[ind,]
mean(train_set$Survived==1)

#guessing if a person survived or not in the test_set
set.seed(3, sample.kind = "Rounding")
surv_guess <- sample(c(0,1),size=nrow(test_set), replace = T)%>%
  factor(levels=levels(test_set$Survived))
mean(surv_guess == test_set$Survived)

#Use the training set to determine whether members of a given sex were 
#more likely to survive or die
train_set %>% group_by(Sex,Survived) %>% 
  summarize(nb=n())
#Or directly have proportion
train_set %>%
  group_by(Sex) %>%
  summarize(Survived_prop = mean(Survived == 1))

surv_sex_pred <- if_else(test_set$Sex =="female",1,0) %>% 
  factor(levels=levels(test_set$Survived))
mean(test_set$Survived == surv_sex_pred)

#Predicting survival by passenger class
test_set %>% group_by(Pclass) %>% 
  summarize(survived_prop= mean(Survived==1))

surv_class_pred <- if_else(test_set$Pclass==1,1,0)%>%
                  factor(levels = levels(test_set$Survived))
mean(surv_class_pred == test_set$Survived)

#Predict survival using both sex and passenger class 
train_set %>% group_by(Sex,Pclass) %>% 
  summarize(survived_prop= mean(Survived == 1))
surv_both_pred <- if_else(test_set$Sex=="female",
                          if_else(test_set$Pclass %in% c(1,2),1,0 ),0) %>%
  factor(levels = levels(test_set$Survived))
mean( surv_both_pred==test_set$Survived)

#confusion matrix
a <-confusionMatrix(surv_sex_pred ,test_set$Survived)
b<- confusionMatrix(surv_class_pred ,test_set$Survived)
c<- confusionMatrix(surv_both_pred ,test_set$Survived)

#F1 scores
F_meas(surv_sex_pred ,test_set$Survived)
F_meas(surv_class_pred ,test_set$Survived)
F_meas(surv_both_pred ,test_set$Survived)

#Survival by fare - LDA and QDA
library(caret)
set.seed(1, sample.kind = "Rounding")
fit_lda <- train(Survived ~ Fare,data=train_set,method="lda")
confusionMatrix( predict(fit_lda,test_set), test_set$Survived)

set.seed(1, sample.kind = "Rounding")
fit_qda <- train(Survived ~ Fare,data=train_set,method="qda")
confusionMatrix( predict(fit_qda,test_set), test_set$Survived)

#Logistic regression models by age
set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ Age,data=train_set,method="glm")
confusionMatrix( predict(fit_glm,test_set), test_set$Survived)

set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ Age+Sex+Fare+Pclass,data=train_set,method="glm")
confusionMatrix( predict(fit_glm,test_set), test_set$Survived)

set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ . ,data=train_set,method="glm")
confusionMatrix( predict(fit_glm,test_set), test_set$Survived)

#knn model
set.seed(6, sample.kind = "Rounding")
fit_knn <- train(Survived ~ . ,
                 data=train_set,method="knn",
                 tuneGrid= data.frame(k = seq(3, 51, 2)))
plot(fit_knn)
confusionMatrix( predict(fit_knn,test_set), test_set$Survived)$overall

#knn model with 10 cross validation
set.seed(8, sample.kind = "Rounding")
#10-fold cross validation; we have 10 samples using 10% of the observations each
ctrl <- trainControl(method="cv",number=10,p=0.9) 

fit_knn <- train(Survived ~ . ,
                 data=train_set,method="knn",
                 tuneGrid= data.frame(k = seq(3, 51, 2) ),
                 trControl= ctrl )
plot(fit_knn)
confusionMatrix( predict(fit_knn,test_set), test_set$Survived)$overall

#rpart model
set.seed(10, sample.kind = "Rounding")
fit_rpart <- train(Survived ~ . ,
                 data=train_set,method="rpart",
                 tuneGrid= data.frame( cp = seq(0, 0.05, 0.002) ) )
plot(fit_rpart)
confusionMatrix( predict(fit_rpart,test_set), test_set$Survived)$overall

#Inspect the final model and plot the decision tree
plot(fit_rpart$finalModel,margin = 0.1)
text(fit_rpart$finalModel,cex=0.75)

#Random forest model
library(randomForest)
set.seed(14, sample.kind = "Rounding")
fit_rf <- train(Survived ~ . ,
                   data=train_set,method="rf",
                   tuneGrid= data.frame( mtry = 1:7 ),
                   ntree= 100)
plot(fit_rf)
confusionMatrix( predict(fit_rf,test_set), test_set$Survived)$overall
#Use varImp to random forest model object to determine the importance of predictors
varImp(fit_rf)
