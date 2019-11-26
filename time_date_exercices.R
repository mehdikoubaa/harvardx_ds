library(tidyverse)
library(dslabs)
library(lubridate)
data("brexit_polls")
tab <- brexit_polls
n_month <- tab %>% filter(month(startdate) == 4 ) %>% summarize(n())
n_week <- tab %>% mutate(week_polls = round_date(enddate,"week")) %>% 
  filter(week_polls == round_date(ymd("2016-06-12"),"week"))
n_days <- tab %>% 
  mutate(num_day=weekdays(enddate)) %>% 
  group_by(num_day) %>% summarize (nbre=n())%>% arrange(nbre)

table(weekdays(brexit_polls$enddate)) #solution proposed

#________other example
data("movielens")
tb <- movielens %>% mutate(date=as_datetime(timestamp))
tb%>%group_by(year(date))%>%summarize(nbre=n())%>% arrange(desc(nbre))
tb%>%group_by(hour(date))%>%summarize(nbre=n())%>% arrange(desc(nbre))
