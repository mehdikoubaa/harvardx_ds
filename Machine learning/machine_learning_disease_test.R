library(tidyverse)
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#probability that a test is positive?
mean(test==1)

#probability that an individual has the disease if the test is negative
mean(disease[test==0])

#probability that you have the disease if the test is positive
mean(disease[test==1])

#If the test is positive, what is the relative risk of having the disease?
mean(disease[test==1])/mean(disease)
