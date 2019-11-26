options(digits = 3)    # report 3 significant digits
library(tidyverse)

#with penalty gessing
p_true<-1/5
p_false<-4/5
nb_quest<-44
E1q<-1*p_true-0.25*p_false
E1q
E44q<-44*E1q
E44q
s1<-sqrt(p_true*p_false)*abs(1+0.25)
s1
s44<-sqrt(44*s1^2)
s44
set.seed(21)
S<-replicate(10000,{
  v<-sample(c(1,-0.25),44,replace=TRUE,prob=c(p_true,p_false))
  sum(v)
})
mean(S>=8)
1-pnorm(8,E44q,s44)

#Without penalty gessing
p_true<-1/4
p_false<-3/4
nb_quest<-44
E1q<-1*p_true+0*p_false
E1q
E44q<-44*E1q
E44q
s1<-sqrt(p_true*p_false)*abs(1)
s1
s44<-sqrt(44*s1^2)
s44
set.seed(21)
S<-replicate(10000,{
  v<-sample(c(1,0),44,replace=TRUE,prob=c(p_true,p_false))
  sum(v)
})
mean(S>=8)
1-pnorm(30,E44q,s44)

#with probability vector
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])





