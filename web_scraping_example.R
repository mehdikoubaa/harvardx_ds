library(tidyverse)
library(rvest)

url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"

#see what tables contained in the web page
test <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table()

#choose the desired table
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

#remove comas
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
