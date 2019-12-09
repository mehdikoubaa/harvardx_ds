library(tidyverse)
library(dslabs)
data("movielens")

#Q1
# Compute the number of ratings for each movie and then plot it against the year 
# the movie came out. Use the square root transformation on the counts.
# What year has the highest median number of ratings?
str(movielens)
n_movies <- movielens %>% group_by(movieId,year)%>%
  summarise(nbre_movies=n())

n_movies%>% ggplot(aes(nbre_movies,year))+geom_point()
n_movies <- as_tibble(n_movies)
median(n_movies$nbre_movies)

# solution given
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Q2
# Among movies that came out in 1993 or later, select the top 25 movies with
# the highest average number of ratings per year (n/year), and caculate the average
# rating of each of them. To calculate number of ratings per year, use 2018 as the 
# end year.

v <- movielens%>%filter(year>=1993 & year<=2018)%>%
  group_by(movieId,year)%>%
  summarize(n_per_year=n(),avg_rating=mean(rating),title=first(title))%>%
              arrange(desc(n_per_year))%>% as_tibble()

#solutoin given
c <- movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 



movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

library(lubridate)
movielens <- mutate(movielens, date = as_datetime(timestamp))

# Compute the average rating for each week and plot this average against day.
# Hint: use the round_date function before you group_by
b <- movielens %>% mutate(weeks= week(round_date(date)))%>%
  group_by(weeks)%>%
  summarize(avg_r=mean(rating))%>% ggplot(aes(weeks,avg_r))+geom_point()

#solution_given
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

#Q6
p <- movielens %>% group_by(genres) %>% 
  summarise(avg_r=mean(rating), sd=sd(rating), n=n())%>% 
  filter(n>=1000) %>%
  arrange(desc(sd))

#solution given
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
          