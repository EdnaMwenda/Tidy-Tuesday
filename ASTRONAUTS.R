#Load packages


library(tidyverse)
library(janitor)
library(knitr)
library(lubridate)
library(ggforce)
library(scales)
library(gganimate)
library(ggblur)
library(jpeg, "png")

tuesdata <- tidytuesdayR::tt_load('2020-07-14')
tuesdata <- tidytuesdayR::tt_load(2020, week = 29)

astronauts <- tuesdata$astronauts


View(astronauts)
str(astronauts)
astro1<-astronauts %>% select(
               name,sex,year_of_birth,nationality,military_civilian,
                    occupation,mission_number,
              total_number_of_missions,year_of_mission,ascend_shuttle,
                        descend_shuttle, year_of_selection

) %>% mutate(Years_Mission= case_when(year_of_mission %in% 1960:1970 ~"1960-1970",
                              year_of_mission %in% 1971:1980 ~"1971-1980",
                              year_of_mission %in% 1981:1990 ~"1981-1900",
                              year_of_mission %in% 1991:2000 ~"1960-2000",
                              year_of_mission %in% 2001:2010 ~"2001:2010",
                              year_of_mission %in% 2011:2020 ~"2011:2020",
)
                              ) 

astro1 %>% count(Years_Mission) 


#remove missing values
sapply(astronauts,function(x) sum(is.na(x)))

astro<-na.omit(astro1);astro

head(astro,n=5)


class(astro$year_of_mission)

astro$year_of_mission<-as.character(astro$year_of_mission)
year(as.POSIXct("2011/2012", format="%Y"))




#set theme
fontastro<-"Gothic"
mine.theme<-theme(
  text = element_text(family = fontastro),
  plot.background = element_rect(fill= "black"),    # Background of the entire plot
  panel.background = element_rect(fill= "#0b3d91"),   # Background of plotting area
  panel.border = element_blank(),       # Border around plotting area.
  # fill argument should be NA
  
  panel.grid = element_blank(),         # All grid lines
  panel.grid.major.y = element_line(colour= "white"),   # Major grid lines
  panel.grid.minor = element_blank(),   # Minor grid lines
  axis.text.y = element_text(size= 11,colour = "white")
  
)






# Let's get plotting -------------------------------------------------------
#The disparity in gender of astronauts
 a<- astro1 %>% 
  ggplot(aes(y=Years_Mission, x=total_number_of_missions, fill= sex))+
  geom_col()+
  geom_text(position = "identity", size=8, label= "")+
 labs(title = "The disparity of female astronauts against men", subtitle = "In decades since 1960",
      x= "Total Number of Missions", y="Years" )+
  theme(
    text = element_text(family = fontastro),
    plot.background = element_rect(fill= "white"),    # Background of the entire plot
    panel.background = element_rect(fill= "white"),   # Background of plotting area
    panel.border = element_blank(),       # Border around plotting area.
    # fill argument should be NA
    
    
    
  )
  
a
  
  space<-readJPEG("C:/Users/Mwenda/Desktop/DekutR/Tidy Tuesday/nasa-sls-1-1280x720.jpg")
 ggbackground(space) 
  
 #The age the ASTRONAUTS were when they were first selected
 Age_atselection = ((astronauts$year_of_selection-astronauts$year_of_birth))
 Age_atselection
 
 
 ggplot(astro1, aes(Age_atselection, year_of_selection, fill= "sex", label= name))+
     geom_point_blur(aes(blur_alpha = 0.3, blur_steps =20))+
     
     theme_dark()+
     theme(
       plot.background = element_rect(fill= "black"),    # Background of the entire plot
       panel.background = element_rect(fill= "white")  # Background of plotting area
     )
    
  
  
  
  
   
   
 ggplot(astro1, aes(Age_atselection, year_of_selection))+
   ## curves
   ggforce::geom_link(
     aes(
       x = Age_atselection, 
       xend = Age_atselection,
       y = 0,
       yend = year_of_selection,
       color = year,
       color = after_scale(colorspace::desaturate(color, .3)),
       alpha = hours
     )
     
     
