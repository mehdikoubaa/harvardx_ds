library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
#African countries: comparaison life expantancy Vs fertility in 2012
gapminder %>% filter(continent=="Africa" & year==2012) %>%
  ggplot(aes(y=life_expectancy,x=fertility,color=region)) +
  geom_point()

#(top african countries in life expantancy and fertility
df<-gapminder%>%
  filter(continent=="Africa",year==2012,fertility<=3,life_expectancy>=70)%>%
  select(country,region,fertility,life_expectancy)%>%arrange(desc(life_expectancy))

#comparaison life expectancy USA - Vietnam 1955 -2010
tab<-gapminder%>% filter(year%in%1960:2010,country%in%c("Vietnam","United States"))
p <- tab%>%ggplot(aes(year,life_expectancy,color=country))+
  geom_line()
p

#cambodia evolution expectancy life
camb_plot<-gapminder%>%filter(year%in% 1960:2010,country=="Cambodia")%>%
  ggplot(aes(year, life_expectancy))+ geom_line()
camb_plot

#daydollars in african countries 2010
daydollars <- gapminder%>%
  mutate(dollars_per_day=gdp/population/365)%>%
  filter(continent=="Africa",year==2010,!is.na(gdp),!is.na(population))

daydollars_p<-daydollars%>% ggplot(aes(dollars_per_day))+
  geom_density() +scale_x_continuous(trans="log2")

#comparaison 1970 - 2010 african countries
gapminder%>%
  mutate(dollars_per_day=gdp/population/365)%>%
  filter(continent=="Africa",year%in%c(1970,2010),!is.na(gdp),!is.na(population))%>% 
  ggplot(aes(dollars_per_day,fill=region))+
  geom_density(bw=0.5,position="stack") +scale_x_continuous(trans="log2")+facet_grid(.~year)

#Africa 2010: Infant mortality Vs Dollars/day
install.packages("ggrepel")
library(ggrepel)
gapminder%>%
  mutate(dollars_per_day=gdp/population/365)%>%
  filter(continent=="Africa",year==2010,!is.na(gdp),!is.na(population))%>%
  ggplot(aes(dollars_per_day,infant_mortality,color=region))+
  geom_point(size=3)+
  geom_text_repel(aes(label=country))+ #use the library ggrepel
  scale_x_continuous(trans="log2",breaks=c(0,0.5,1,2,4,8,16,32))

#Tunisia GDP per capita per day evolution 1960-2011
gapminder%>%mutate(dollars_per_day=gdp/population/365)%>%
  filter(year%in% 1960:2011,country=="Tunisia",!is.na(dollars_per_day))%>%
  ggplot(aes(year,dollars_per_day ))+ 
  geom_line(size=1.5)+
  labs(title="Tunisia GDP per Capita per Day (1960-2011)",x="Year",y="Dollars/Day")+
  theme_bw()
  
 
