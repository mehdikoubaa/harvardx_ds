library(tidyverse)
library(gutenbergr)
library(tidytext)
library(textdata)
options(digits = 3)

#try to solve
metagut <- gutenberg_metadata
v <-str_detect(metagut$title,"Pride and Prejudice")
id <- metagut[v,]
id2<-na.omit(id,id$title)

#¼solution given
id3 <- metagut %>% filter(str_detect(title,'Pride and Prejudice'))

id4 <- gutenberg_works(title=="Pride and Prejudice",languages = "en")

txt <- gutenberg_download(1342)
words <- txt %>% unnest_tokens(word,text)
nrow(words)
words2 <- filter(words,!words$word %in% stop_words$word) #remove stop words
nrow(words2)
words3 <- words2 %>% filter(!str_detect(word,"\\d")) #remove digits

anal <- words3 %>% group_by(word)%>% 
  summarize (nbre= n()) %>% 
  arrange (desc(nbre)) %>% filter(nbre>=100)

afinn <- get_sentiments("afinn")
affin_sentiment <- words3 %>% inner_join(afinn) #join tables
nb <- affin_sentiment %>% filter(value>0)%>% summarize(n()/nrow(words3)) #positive sentitment
affin_sentiment %>% filter(value==4)%>% count() #count value 4
