

# You will need the following libraries for the assignment:

library(readr) # fileformat of the dataset
library(ggplot2)     # the ggplot package
library(tidyverse)  # the tidyverse package


# To carry out the assignment, you will need to combine the union_unempl data with map data. 


union<- read_csv("https://uit-sok-2008-h22.github.io/Assets/union_unempl.csv") #This loads the data with information about the variables of interest
View(union) #Displays the data
#To combine the unemployment and union data with the map data, we merge on country name. 
#We face two problems here: 1) United Kingdom is called "UK" in the map data, 2) the variable "country" is called "region" in the map data. We need to change this.

#Changing the name of a single observation. The below code changes all observations called "United Kingdom" to "UK" in the union data. 
union$country <- gsub("United Kingdom", "UK", union$country)
View(union) 

# Renaming a variable. The below code renames the variable "Country" to "Region".
names(union)[names(union) == "country"] <- "region"
View(union) 

# Creating a new variable. To create a map showing "Excess coverage", you need to create a new variable. The below code shows how to create a new variable in R. 
mapdata <- union %>% 
  mutate(excess_coverage = union$coverage-union$density)

# You are now ready to create your maps! Follow the tutorial at https://www.youtube.com/watch?v=AgWgPSZ7Gp0 

# The "Coord" variable takes 5 discrete levels. It may therefore be better to use a discrete scale for the coloring. 
# To do this, simply replace "scale_fill_gradient(name="name", low="color1", high="color2", na.value="grey50")" with "scale_fill_brewer(name="name", palette = "Set1")" (or another set you prefer)

mapdata <- map_data("world")
mapdata <- left_join(mapdata,union, by="region")
mapdata <- mapdata %>%
  filter(!is.na(mapdata$iso3c)) 
mapdata %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=unempl), color = "black") + 
  scale_fill_gradient(name="% Arbeidsledighet", low="blue", high="red") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank()) +
        ggtitle("Arbeidsledigheten i Europa")


mapdata %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=density), color = "black") + 
  scale_fill_gradient(name="% Fagforening", low="blue", high="red") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank()) +
  ggtitle("Andelen av sysselsatte, medlem fagforening")


mapdata %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=coverage), color = "black") + 
  scale_fill_gradient(name="% Excess covrage", low="blue", high="red") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank()) +
  ggtitle("Andelen dekkes av kollektive forhandlinger")

mapdata %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=coord), color = "black") + 
  #scale_fill_gradient(name="% Excess covrage", low="blue", high="red") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank()) +
  ggtitle("Hvordan det forhandles")
