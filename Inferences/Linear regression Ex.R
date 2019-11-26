library(tidyverse)

#___________base ball exercice
library(Lahman)
data("Teams")
?Teams
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(E_per_game = E/G, W_per_game = W/G) %>%
  ggplot(aes(E_per_game, W_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>% 
  summarize(r=cor(AB_per_game,R_per_game))

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(E_per_game = E/G, W_per_game = W/G) %>%
  summarize(r=cor(W_per_game,E_per_game))

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  summarize(r=cor(X3B_per_game,X2B_per_game))

#_______________Galton exercice
library(HistData)
data("GaltonFamilies")
set.seed(1989)
options(digits = 3)
female_heights <- GaltonFamilies%>%    
  filter(gender == "female") %>%    
  group_by(family) %>%    
  sample_n(1) %>% #sample_n :: dyplyr fct to sample rows from table, if grouped sample a row from each group
  ungroup() %>%    
  select(mother, childHeight) %>%    
  rename(daughter = childHeight)

#  Calculate the mean and standard deviation of mothers' heights, ../..
#  the mean and standard deviation of daughters' heights, ../..
#  and the correlaton coefficient between mother and daughter heights. //

param <- female_heights %>% summarise(mu_m= mean(mother),
                                      mu_d=mean(daughter),
                                      sd_m=sd(mother),
                                      sd_d=sd(daughter),
                                      rho=cor(mother,daughter))

reg <- param %>% summarise(slope=rho*(sd_d/sd_m),intercept=mu_d-slope*mu_m)

lm(mother~daughter,female_heights)
fit <- c(0.31,44.2)#take number of the desired answer
pred <- female_heights %>% mutate(pred_mother=fit[1]*daughter+fit[2])

#_________LSE galton___
set.seed((1983))
galton_heights <- GaltonFamilies%>%    
  filter(gender == "male") %>%    
  group_by(family) %>%    
  sample_n(1) %>% #sample_n :: dyplyr fct to sample rows from table, if grouped sample a row from each group
  ungroup() %>%    
  select(father, childHeight) %>%    
  rename(son = childHeight)

#risidual sum of squares
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

fit <-  lm(son ~ father,galton_heights) #class >> list [12]
g <- fit$coef #class >> vector

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
}) #lse matrix num [1:2,1:1000]


lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

set.seed(1)
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>% group_by(pair)%>%summarise(correlation=cor(childHeight,parentHeight))
library(broom)
galton %>% group_by(pair)%>%do(tidy(lm(childHeight~parentHeight,.),conf.int=T))
galton %>% group_by(pair)%>%
  do(tidy(lm(childHeight~parentHeight,.),conf.int=T))%>%
  arrange(p.value)%>% mutate(interval=conf.high-conf.low)


#___LSE for baseball
library(Lahman)
library(broom)
data("Teams")

pred <- Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(RpG = R/G ,HRpG = HR/G, BBpG = BB/G)%>%
  select(RpG,BBpG,HRpG) %>% lm(RpG~BBpG+HRpG,.)

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99.01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%group_by(playerID)%>%
  summarize(mean_bb=mean(bb),mean_singles=mean(singles))

bat_99.01 %>% filter(mean_singles>0.2)%>%summarise(n())
bat_99.01 %>% filter(mean_bb>0.2)%>%summarise(n())

inter <- inner_join(bat_02,bat_99.01)
cor(inter$singles,inter$mean_singles)
cor(inter$bb,inter$mean_bb)

inter %>% ggplot(aes(singles,mean_singles))+
  geom_point(alpha=0.5)
inter %>% ggplot(aes(bb,mean_bb))+
  geom_point(alpha=0.5)
inter%>%lm(bb~mean_bb,.)
inter%>%lm(singles~mean_singles,.)

pred1 <- Teams %>% filter(yearID %in% 1961:2018 ) %>% 
  group_by(yearID)%>%
  do(tidy(lm(R~BB+HR,.),conf.int=T))%>%ungroup
pred1 %>%filter(term=="BB")%>% 
  ggplot(aes(yearID,estimate))+geom_point(alpha=0.5)+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high))+
  geom_smooth(method = "lm")

pred2 <- pred1 %>% filter(term=="BB")
pred2 <- pred1 %>% filter(term=="BB")%>%
  do(tidy(lm(estimate~yearID,.),conf.int=T))
pred2
