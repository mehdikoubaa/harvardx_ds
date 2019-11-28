library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)
library(dslabs)
library(caret)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", 
                  package="dslabs")

dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4,
                        "MAY" = 5, "JUN" = 6, "JUL" = 7, "AGO" = 8,
                        "SEP" = 9,"OCT" = 10,"NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

#___smooth and plot death vs date (degree=1)
# try geom_smooth
ggplot(dat,aes(date,deaths))+
  geom_point()+
  geom_smooth( method.args = list(degree=1),span=0.05)

#try loess
span <- 2*30/(diff(as.numeric(range(dat$date))))

fit <- loess(deaths ~ as.numeric(date), dat, degree = 1,span = span)
dat2 <- dat %>% mutate(smooth=predict(fit,.))

dat2 %>% 
  ggplot(aes(date,deaths))+
  geom_point()+
  geom_line(aes(date,smooth),size=2,col="red")


dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)
  
