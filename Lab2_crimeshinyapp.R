library(ggplot2)
library(ggmap)
library(tidyverse)
library(dplyr)

crimecsv <- read.csv("Lab 7 - crimeInSYR-1.csv")

crimecsv <- data.frame(crimecsv)


crimecsv <- crimecsv %>%
                  rename(
                    Type = Aggravated.assault,
                    Address = X800.block.Avery.Av,
                    City = Syracuse,
                    Date = X12.20.15
                    
                  )

crimecsv$Address <- as.character(crimecsv$Address)
crimecsv$Type <- as.character(crimecsv$Type)
crimecsv$City <- as.character(crimecsv$City)
crimecsv$Date <- as.character(crimecsv$Date)



##===========================================================================


address_complete <- merge(crimecsv$Address, crimecsv$City)
address_complete

crimecsvaddi <- geocode(crimecsv$Address)
crimecsv$coor <-crimecsvaddi

##=====================================================================================
syr <- geocode("syracuse university, syracuse, ny")
syr

syr.map <- get_map(location = syr, zoom = 11)
mapSimple <- ggmap(syr.map) +
  geom_point(aes(x = lon, y = lat), data = crimecsvaddi, colour = "blue", size = 1)
mapSimple


