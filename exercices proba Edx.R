library(gtools)
library(tidyverse)
options(digits = 3)


runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands",
             "France", "South Africa")
set.seed(2)
B<-10000
Pr3Jam<-replicate(B,{
  v<-sample(runners,3)
!any(v==c("Jamaica", "Jamaica", "Jamaica"))
})
mean(Pr3Jam)
___________________________________
combinations(6,1)#entries
combinations(6,2)#sides
combinations(3,1)#drinks
nbcomb<- nrow(combinations(6,1))*nrow(combinations(6,3))*nrow(combinations(3,1))

nc<- function(n){
  nrow(combinations(6,1))*nrow(combinations(n,2))*nrow(combinations(3,1))
}
v<-seq(2,12,1)
sapply(v,nc)
min(which(sapply(v,nc)>360))+1
___________________________________
library(tidyverse)
data("esoph")
all_cases<-sum(esoph$ncases)
all_controls<-sum(esoph$ncontrols)
esoph%>%group_by(alcgp)%>%
  summarize(nbre_cases=sum(ncases),nbre_controls=sum(ncontrols),
            total=nbre_cases+nbre_controls,
            prob=nbre_cases/total)

esoph%>%group_by(tobgp)%>
  summarize(nbre_case=sum(ncases),nbre_control=sum(ncontrols),
            total=nbre_case+nbre_control)
esoph%>%filter(as.numeric(tobgp)%in% 2:4)%>%summarize(sum(ncases)/all_cases)
esoph%>%filter(as.numeric(tobgp)%in% 2:4)%>%summarize(sum(ncontrols)/all_controls)

esoph%>%group_by(alcgp)%>%
  summarize(nbre_cases=sum(ncases),prob_case=nbre_cases/all_cases,nbre_controls=sum(ncontrols),
            prob_control=nbre_controls/all_controls)

tab1<-esoph%>%
  group_by(alcgp,tobgp)%>%
  summarize(nbre_cases=sum(ncases),prob_case=nbre_cases/all_cases,nbre_controls=sum(ncontrols),
            prob_control=nbre_controls/all_controls)
probalc<- tab1%>%filter(alcgp=="120+")%>%summarize(r=sum(prob_case)) %>% .$r
probtob<-tab1%>%filter(as.numeric(tobgp)==4)%>%summarize(s=sum(prob_case))%>%.$s
pr<-probalc+sum(probtob) - (tab1%>%filter(tobgp=="30+" & alcgp=="120+")%>%summarize(sb=sum(prob_case))%>%.$sb)

esoph%>%group_by(tobgp)%>%
  summarize(nbre_cases=sum(ncases),prob_case=nbre_cases/all_cases,nbre_controls=sum(ncontrols),
            prob_control=nbre_controls/all_controls)

tab1<-esoph%>%
  group_by(alcgp,tobgp)%>%
  summarize(nbre_cases=sum(ncases),prob_case=nbre_cases/all_cases,nbre_controls=sum(ncontrols),
            prob_control=nbre_controls/all_controls)
probalc2<- tab1%>%filter(alcgp=="120+")%>%summarize(r=sum(prob_control)) %>% .$r
probtob2<-tab1%>%filter(as.numeric(tobgp)==4)%>%summarize(s=sum(prob_control))%>%.$s
pr2<-probalc2+sum(probtob2) - (tab1%>%filter(tobgp=="30+" & alcgp=="120+")%>%summarize(sb=sum(prob_control))%>%.$sb)
