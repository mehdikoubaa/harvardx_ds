library(tidyverse)
options(digits = 10)
# # An education expert is advocating for smaller schools.
# # The expert bases this recommendation on the fact that among 
#   the best performing schools,many are small schools. 
# #  Let's simulate a dataset for 1000 schools. 
# # First, let's simulate the number of students in each school:

set.seed(1986, sample.kind="Rounding")
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely independent 
# from size. This is the parameter we want to estimate in our analysis
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
#top 10 school
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# # Now let's have the students in the school take a test. There is random 
# variability in test taking, so we will simulate the test scores as normally 
# distributed with the average determined by the school quality with a 
# standard deviation of 30 percentage points.

set.seed(1, sample.kind="Rounding")

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1
# What are the top schools based on the average score? Show just the 
# ID, size, and the average score.
# Report the ID of the top school and average score of the 10th school.

schools %>% arrange (desc(score))%>% top_n(10,score)

#Q2
median(schools$size)
schools %>% arrange(desc(score))%>% top_n(10,wt=score) %>% summarize(median(size))
#solution given
schools %>% top_n(10, score) %>% .$size %>% median()

#Q3
median(schools$size)
schools %>% arrange(score)%>% slice(1:10) %>% summarize(median(size))

#Q4
# From this analysis, we see that the worst schools are also small. 
# Plot the average score versus school size to see what's going on.
# Highlight the top 10 schools based on the true quality.

schools %>% ggplot(aes(score,size,col=rank))+ geom_point(alpha = 0.5,highlight=T)
#solution
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

#Q5
overall <- mean(sapply(scores, mean))
alpha <- 25

above_score <- sapply(1:length(scores), function(i){
  sum( scores[[i]] )/(schools$size[i] +alpha)- overall
}) 

# solution given
          score_reg <- sapply(scores, function(x) {
            overall + sum(x-overall)/(length(x)+alpha)
            })
#End solution
schools %>% 
  mutate(score_reg = score_reg) %>% 
  top_n(10,wt=score_reg)%>% arrange(desc(score_reg))

#Q6
RMSE <- function(quality,estimate,n=1000){
  
  sqrt(sum((quality-estimate)^2)/n)
}

alpha <- 10:250

score_regss <- lapply(alpha,function(x){
                    score_reg <- sapply(scores, function(l) {
                      overall + sum(l-overall)/(length(l)+alpha)
                    })
                    score_reg
})

#solution given
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]  

plot(alpha,min_alpha)

#Q7
alpha <- 128
score_reg <- sapply(scores, function(x) {
  overall + sum(x-overall)/(length(x)+alpha)
})
#End solution
schools %>% 
  mutate(score_reg = score_reg) %>% 
  top_n(10,wt=score_reg)%>% arrange(desc(score_reg))

#Q8
# A common mistake made when using regularization is shrinking values towards 0 
# that are not centered around 0. For example, if we don't subtract the overall 
# average before shrinking, we actually obtain a very similar result. Confirm 
# this by re-running the code from the exercise in Q6 but without removing the 
# overall mean.
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]


