library(tidyverse)

#______reshaping_data _________Part 1
d<-read_csv("time.txt")

tidy_data<-d%>% gather(key,value,-age_group)%>%
  separate(key,c("year","variable"),sep="_")

tidy_data2<-d%>% gather(key,value,-age_group)%>%
  separate(key,c("year","variable"),sep="_")

tidy_data3<-d%>% gather(key,value,-age_group)%>%
  separate(key,into=c("year","variable"),sep="_")%>%
  spread(variable,value,)
#____reshaping_data__________Part 2

data("co2")

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy<-co2_wide%>%gather(month,co2,-year)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

data("admissions")
dat <- admissions %>% select(-applicants)

dat2<-dat%>%spread(gender,admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2<-tmp%>%unite(col_name,c(key,gender),sep="_")

tmp3<-tmp2%>%spread(col_name,value)

