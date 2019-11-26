library(tidyverse)
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])
sapply(nodes[1:4], html_table)

html_table(nodes[-3])




tab1<-html_table(nodes[[10]])
tab1<-tab1%>%select(-X1)%>%slice(-1)
names(tab1)<-c("Team", "Payroll", "Average")

tab2<-html_table(nodes[[19]])
tab2<-tab2%>%slice(-1)
names(tab2)<-c("Team", "Payroll", "Average")


tab3<-full_join(tab1,tab2,by="Team")


#------------suite
url2 <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab<-html_nodes(read_html(url2),"table")
h2<-html_table(tab[1:5],fill=T)
