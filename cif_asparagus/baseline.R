library(tidyverse)
library(decisionSupport)


#easy base model####
quan_100<-vv(yield, var_CV, n)
qual_100<-100

chill_requirement<-chance_event(chill_req)
warm_spring<-chance_event(warm_spring)
late_frost<-chance_event(late_frost)