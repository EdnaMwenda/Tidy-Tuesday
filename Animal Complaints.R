
#Load the packages

library(tidytuesdayR)
library(tidyverse)
library(lubridate)


#LOAD THE DATA

tuesdat<-tidytuesdayR::tt_load('2020-07-21')
 


# USING ANIMAL COMPALINTS DATA --------------------------------------------

anim_comp<-tuesdat$animal_complaints

glimpse(anim_comp)

head(anim_comp,n=5)
tail(anim_comp,n=5)



#remove missing values
sapply(anim_comp,function(x) sum(is.na(x)))

###Hurray no missing values


# Let's get plotting!!!! ------------------------------------------------------------


anim_comp %>% ggplot(aes(y= `Date Received`, x=`Complaint Type`, fill= "Animal Type"))+
geom_col()










