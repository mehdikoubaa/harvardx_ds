library(tidyverse)
library(caret)
library(dslabs)
data("movielens")

# create data partition
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

# To make sure we don’t include users and movies in the test set that do not appear
# in the training set,we remove these entries using the semi_join function:
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") 

# Return rows of test_set that have a match in train_set example:
# df1 <- data.frame(x=1:10,z=6:15)
# df2 <- data.frame(x=seq(1,10,2),w=seq(6,15,2))
# semi_join(df1,df2,by="x")

#RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#the equation of estimating the rating should be:
# y_hat=mu+b_i+b_u+eps_i_u

mu <- mean(train_set$rating) 

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

RMSE(predicted_ratings, test_set$rating)
#> [1] 0.905

# Regularization
# add lambda variable that take in consideration number of ratings per movie

#estimating lambda 
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
          mu <- mean(train_set$rating)
          
          b_i <- train_set %>% 
            group_by(movieId) %>%
            summarize(b_i = sum(rating - mu)/(n()+l))
          
          b_u <- train_set %>% 
            left_join(b_i, by="movieId") %>%
            group_by(userId) %>%
            summarize(b_u = sum(rating - b_i - mu)/(n()+l))
          
          predicted_ratings <- 
            test_set %>% 
            left_join(b_i, by = "movieId") %>%
            left_join(b_u, by = "userId") %>%
            mutate(pred = mu + b_i + b_u) %>%
            pull(pred)
          
          return(RMSE(predicted_ratings, test_set$rating))
})

lambda <- lambdas[which.min(rmses)]
lambda
#> [1] 3.25

# ==== matrix factorization =====
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woman used in example
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

# add row names and column names:
rownames(y)<- y[,1]
y <- y[,-1]
colnames(y) <- with(train_small, title[match(colnames(y), movieId)])

#♦  convert them to residuals by removing the column and row effects
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
p1 <- qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
p2 <- qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
p3 <- qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

gridExtra::grid.arrange(p1, p2 ,p3, ncol = 3)

# By looking at the correlation between movies, we can see a pattern
# (we rename the columns to save print space):

x <- y[, c(m_1, m_2, m_3, m_4, m_5)]
short_names <- c("Godfather", "Godfather2", "Goodfellas",
                 "You've Got", "Sleepless")
colnames(x) <- short_names
cor(x, use="pairwise.complete")
#>            Godfather Godfather2 Goodfellas You've Got Sleepless
#> Godfather      1.000      0.829      0.444     -0.440    -0.378
#> Godfather2     0.829      1.000      0.521     -0.331    -0.358
#> Goodfellas     0.444      0.521      1.000     -0.481    -0.402
#> You've Got    -0.440     -0.331     -0.481      1.000     0.533
#> Sleepless     -0.378     -0.358     -0.402      0.533     1.000

# There seems to be people that like romantic comedies more than expected,
# while others that like gangster movies more than expected.


