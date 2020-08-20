tuesdata <- tidytuesdayR::tt_load('2020-07-07')
tuesdata <- tidytuesdayR::tt_load(2020, week = 28)


####OR/AND

raw_arabica <- read_csv("https://raw.githubusercontent.com/jldbc/coffee-quality-database/master/data/arabica_data_cleaned.csv") %>% 
  janitor::clean_names()


coffee_ratings <- tuesdata$coffee_ratings




#LOAD THE PACKAGES

library(tidyverse)
library(lubridate)
library(gganimate)
library(tidyr)


View(coffee_ratings)


glimpse(coffee_ratings)



glimpse(raw_arabica)
View(raw_arabica)

#SELECT COLUMNS TO WORK ON 
arabica<-raw_arabica %>% select(c(species,owner,owner_1,country_of_origin,
                                  region,aroma,flavor,harvest_year,
                                  sweetness,producer,number_of_bags,ico_number,
                                  company))

tail(arabica)

View(arabica)
#Cleaning the data
str(arabica)
tail(arabica$sweetness)



#clean the harvest year column

class(arabica$harvest_year)

arabica$sweetness<-round(as.numeric(arabica$sweetness),0)

#change to the date class


arabica$harvest_year<-as.Date(arabica$harvest_year, format = "%m/%y")





#remove missing values
sapply(arabica,function(x) sum(is.na(x)))

arabica1<-na.omit(arabica)






##Remove Aliases on country and company names
arabica$owner<-gsub("\\=.*","", arabica$owner)

arabica$owner_1<-gsub("\\=.*","", arabica$owner_1)

arabica$country_of_origin<-gsub("\\=.*","", arabica$country_of_origin)


arabica$region<-gsub("\\=.*","", arabica$region)







year(as.POSIXct("2011/2012", format="%Y"))

#Plotting the number of bags that were produced in an harvest year

arabica  %>% na.omit() %>% 
  ggplot(aes(x= harvest_year, y=number_of_bags))+
  geom_point(color="darkorchid4")+
  labs(title = "The number of coffee bags produced in an harvest year",
       y="The number of coffee bags", 
       x=" Harvest Year")+
  theme_bw(base_size = 15)





#Facet wrap to view each year's production individually
arabica  %>% na.omit() %>% 
  ggplot(aes(x= harvest_year, y=number_of_bags))+
  geom_point(color="darkorchid4")+
  labs(title = "The number of coffee bags produced in an harvest year",
       y="The number of coffee bags", 
       x=" Harvest Year")+
  theme_bw(base_size = 15)+
  facet_wrap(~harvest_year)



#Plot the sweetness of the coffee for different regions

arabica %>% 
  na.omit() %>% 
  
  ggplot(aes(x= sweetness, y=country_of_origin))+
  geom_col(color="darkred")+
  labs(title = "Sweetness of coffee in various countries",
       y="Country of Origin", 
       x=" Level of sweetness")+
  theme_bw(base_size = 15)     





# ARABICA Coffee for African Countries

arabica %>% filter(country_of_origin==c("Kenya","Ethiopia","Rwanda","Uganda",
                                        "Cote d'Ivoire","Malawi","Zambia","Mauritius"))













