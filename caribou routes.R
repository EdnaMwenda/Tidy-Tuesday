###Tidy Tuesday
#Data: Caribou Routes

#Load Packages

library(lubridate)
library(tidyverse)
library(geosphere)
library(sf)
library(ggmap)
library(tidytuesdayR)
library(raster)
install.packages("rworldmap")



# Load Data ---------------------------------------------------------------

rm(list = ls())
knitr::opts_chunk$set(echo = TRUE, warning = FALSE,
                      message = FALSE,
                      fig.height = 0.4,
                      fig.width = 0.4)
 
locations<-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')
individuals<-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')



locations %>% group_by(season, study_site)





ggplot()+
   geom_sf()+
   geom_polygon(data= locations, aes(x=longitude , y= latitude), fill= "forestgreen", 
                colour = "skyblue")+
    coord_cartesian()+
   geom_point(data= locations,aes(x=longitude , y= latitude), colour= "yellow" )+
   
   geom_text(data= locations,aes(x=longitude , y= latitude), label= "study_site", size= 3, hjust= "left")+
   labs(x= "Longitude", y= "Latitude")+
   theme_classic()+
   theme_classic()
   
   




head(locations)
 tail(locations)
 
 #Checking the structure of the data
glimpse(locations)
View(locations)
 
#Any missing values?

sapply(locations,function(x) sum(is.na(x)))


# Tidying the data --------------------------------------------------------
#Using Kennedy Site for this analysis

k<-locations %>% 
   #Looking at the Kennedy Study site during variuos seasons
   filter(study_site=="Kennedy") %>% 
   group_by(season,animal_id) %>% 
   count() %>% 
   arrange(desc(n)) %>% ungroup()
   
   

k

plot(k)













































ken<- locations %>% 
  #Looking at the Kennedy Study site
  filter(study_site=="Kennedy", year(timestamp)==2001) %>% filter(as_date(min(timestamp))=="2001-02-21",
            as_date(max(timestamp))=="2001-12-31") %>% 
  ungroup() %>% 
  mutate(date= as_date(timestamp)) %>% 
   group_by(animal_id, date) %>% 
  
 


 ken



 
 
 
 top.anim<-locations %>% group_by(animal_id) %>% 
   count() %>% 
   arrange(desc(n)) %>% 

   ungroup() %>% 
   top_n(5,n)
 
 
 top.anim
 
 

# Visualization -----------------------------------------------------------

p<-ggplot(data=locations)+
   geom_sf()+
   xlab("longitude")+
   ylab("latitude")+
   ggtitle("Caribou",subtitle = paste0("(",length(unique(locations)),"countries"))
           
 
 
 kens<-subset(rowid_to_column(locations,var="row_id"),locations$study_site== "Kennedy")
 q<-qmplot(lon="longitude", lat="latitude",data=kens,colour=I('green'),size=I(3),darken = .5)                            
 
 
 
 pr<-ggplot(locations,aes(x=longitude,y=latitude, colour= season))+
   geom_point()
pr   
 