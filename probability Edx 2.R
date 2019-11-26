library(tidyverse)
set.seed(16,sample.kind = "Rounding")
act_scores<-rnorm(10000,20.9,5.7)
sd(act_scores)
sum(act_scores>=36)
mean(act_scores>=30)
mean(act_scores<=10)

x<-seq(1,36)
f_x<-dnorm(x,20.9,5.7)
plot(x,f_x)

z_scores<-(act_scores-mean(act_scores))/sd(act_scores)
mean(z_scores>2)
2*sd(act_scores)+mean(act_scores)
qnorm(0.975,mean(act_scores),sd(act_scores))

cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))
qnorm(0.95,20.9,5.7)

p<-seq(0.01,0.99,0.01)
sample_quantiles<-quantile(act_scores,p)
sample_quantiles
min(which(sample_quantiles>=26))

theoretical_quantile<-qnorm(p,mean(act_scores),sd(act_scores))
qqplot(theoretical_quantile, sample_quantiles) + geom_abline()
