library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed((22112019))
galton_heights <- GaltonFamilies %>%
  filter( gender=="male") %>% group_by(family)%>%
  sample_n(1)%>%
  ungroup()%>%
  select(father,son=childHeight,childNum,family)


galton_heights <- galton_heights %>%
  mutate(father_centered = father - mean(father))

lm(son ~ father_centered, data = galton_heights) #least square estimate LSE of linear model
summary(lm(son ~ father_centered, data = galton_heights))
