library(dplyr)
library(ggplot2)
library(dslabs)
data("heights")
p<-heights%>%ggplot(aes(x=height*2.54,fill=sex))+
geom_density(alpha=0.5)+
labs(x="height in Cm",y="density")+
facet_grid(~sex)+ theme_bw()

library(gridExtra)
library(ggthemes)
p+theme_economist_white()

#******
library(dplyr)
library(ggplot2)
library(dslabs)
library(ggrepel)
library(gridExtra)
data("murders")
p1 <- murders %>%
  mutate(rate = total/population*10^5) %>%
  filter(population < 2*10^6) %>%
  ggplot(aes(population/10^6, rate, label = abb)) +
  geom_point(size=1.5)+
  geom_text_repel() +
  ggtitle("Small States")

p2 <- murders %>%
  mutate(rate = total/population*10^5) %>%
  filter(population > 10*10^6) %>%
  ggplot(aes(population/10^6, rate, label = abb)) +
  geom_point(size=1.5)+
  geom_text_repel() +
  ggtitle("Large States")

grid.arrange(p1, p2, ncol = 2)