library(gdata)
library(tidyverse)
library(dplyr)
library(readxl)
library(zipcode)
library(rio)
library(devtools)
library(ggplot2)
library(ggmap)
library(maps)
library(mapproj)
library(remotes)
library(zipcode)



##Step 1: Load dataframe and clean dataframe
mydata <- import("https://www.psc.isr.umich.edu/dis/census/Features/tract2zip/MedianZIP-3.xlsx")

str(mydata)

data(zipcode)

mydata$Zip <- clean.zipcodes(mydata$Zip)

dfNew <- merge(mydata,zipcode, by.x ="Zip", by.y = "zip" )



which(dfNew$state == "HI" )
which(dfNew$state == "AK")

dfNew <-dfNew[-31328:-31415,]
dfNew <-dfNew[-32322:-32546,]

dfNew$statenames <- state.name[match(dfNew$state,state.abb)]
dfNew$statenames <- tolower(dfNew$statenames)
##=========================================================================================================================================

##Step 2: Show the income and population per state

dfSimple <- dfNew %>% 
                group_by(state) %>%
                summarize(meanIncome = mean(Median, na.rm = TRUE),meanPop = mean(Pop, na.rm = TRUE))

dfSimple$stateName <- state.name[match(dfSimple$state,state.abb)]
dfSimple$stateName <- tolower(dfSimple$stateName)

us <- map_data("state")

map.simple <- ggplot(dfSimple, aes(map_id = stateName))
map.simple <- map.simple + geom_map(map = us,fill = "white", color = "black" )
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat)
map.simple <- map.simple + coord_map() + 
  ggtitle("Basic Map of Continetal USA")
map.simple

##map with color and average median income 
map.avmedIncome <- ggplot(dfSimple, aes(map_id = stateName))
map.avmedIncome <- map.avmedIncome + geom_map(map = us, aes(fill = dfSimple$meanIncome))
map.avmedIncome <- map.avmedIncome + expand_limits(x = us$long, y = us$lat)
map.avmedIncome <- map.avmedIncome + coord_map() + ggtitle("Average Median Income of the U.S.")
map.avmedIncome
## map representing the average population

map.pop <- ggplot(dfSimple, aes(map_id = stateName))
map.pop <- map.pop + geom_map(map = us, aes(fill = dfSimple$meanPop))
map.pop <- map.pop + expand_limits(x = us$long, y = us$lat)
map.pop <- map.pop + coord_map() + ggtitle("Average Median Population")
map.pop
##==========================================================================================================
##Step 3: SHow the income per state
## there is not column called statename in dfnew
map.incomeperzip <- ggplot(dfNew, aes(map_id = statenames)) + 
                        geom_map(map = us, color = "white", fill = "black") +
                        geom_point(data = dfNew, aes(x = longitude, y = latitude, color = Median)) +
                        expand_limits(x = us$long, y = us$lat) +
                        ggtitle("Median Income Per State")
map.incomeperzip
##=================================================================================================================  
##Step 4: Show zip code density
map.incomeperzip <- ggplot(dfNew, aes(map_id = statenames)) + 
                        geom_map(map = us, color = "white", fill = "black") +
                        geom_point(data = dfNew, aes(x = longitude, y = latitude, color = Median)) +
                        stat_density2d(aes(x = longitude, y = latitude)) +
                        expand_limits(x = us$long, y = us$lat) +
                        ggtitle("Median Income Per State Density")

map.incomeperzip

##===================================================================================================================
##Step:5 

api <-"AIzaSyADUIYbKxM_DiUS22WeHEBoRvUDM9FhjT0"
register_google(key = api)
latlon <- geocode("NYC,ny")
##===========================================================================================================================
mapZipZoomed <- map.incomeperzip + geom_point(aes(x = latlon$lon, y = latlon$lat), color = "darkred", size = 3)
mapZipZoomed <- map.incomeperzip + xlim(latlon$lon - 10, latlon$lat + 10) + 
  ylim(latlon$lat - 10, latlon$lat + 10) + coord_map()
mapZipZoomed
##==============================================================================================================================
map.incomezipzoom <- map.incomeperzip + geom_point(aes(x = latlon$lon, y = latlon$lat), color = "darkred", size = 3) 
map.incomezipzoom <- map.incomeperzip + xlim(latlon$lon, latlon$lon + 10) + ylim(latlon$lat - 10,latlon$lat + 10) + coord_map() 
map.incomezipzoom  
