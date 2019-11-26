options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
#___________exercice 2
titanic%>%ggplot(aes(Age,color=Sex))+
    #geom_density(alpha=0.5)+
    geom_histogram(binwidth = 2)+
    facet_grid(.~Sex)
#____________exercice 3
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic%>%ggplot(aes(sample=Age))+
  geom_qq(dparams=params)+
  geom_abline()

#____________exercice 4
titanic%>%
  ggplot(aes(Survived,fill=Sex))+
  geom_bar(position = position_dodge())

#____________exercice 5
titanic%>%
  ggplot(aes(Age,fill=Survived))+
  geom_density(alpha=0.2,aes(y=..count..))

#____________exercice 6
titanic%>%filter(Fare!=0)%>%
  ggplot(aes(Survived,Fare))+
  geom_boxplot()+
  scale_y_continuous(trans="log2")+
  geom_jitter(alpha=0.1)

#____________exercice 7
titanic%>%ggplot(aes(Pclass,fill=Survived))+
  geom_bar()
titanic%>%ggplot(aes(Pclass,fill=Survived))+
  geom_bar(position=position_fill())
#___________exercice 8
titanic%>%
  ggplot(aes(Age,fill=Survived))+
  geom_density(alpha=0.5,aes(y=..count..))+
  facet_grid(Sex~Pclass)

  