library(ggplot2)
library(reshape2)
library(tidyverse)

## Step 1: Loading data
air <- data.frame(airquality)
air

##Step 2: Clean the data
meanofaq <- mean(air$Ozone, na.rm = TRUE)
air$Ozone[which(is.na(air$Ozone))] <- meanofaq

meanofSolar <- mean(air$Solar.R, na.rm = TRUE)
air$Solar.R[which(is.na(air$Solar.R))] <- meanofSolar
air

##Step 3: Understanding teh data distribution
##histogram(Ozone)
ggplot(air, aes(Ozone)) +
  geom_histogram()

##histogram(Solar.R)
ggplot(air,aes(Solar.R)) +
  geom_histogram()

##histogram(Wind)
ggplot(air, aes(Wind)) +
  geom_histogram()

##histogram(Temp)
ggplot(air, aes(Temp)) +
  geom_histogram()

##histogram(Month)
ggplot(air, aes(Month)) +
  geom_histogram()

##histogram(Day)
ggplot(air, aes(Day)) +
  geom_histogram()

##boxplot for Ozone and boxplots for different wind values
breaks <- c(0,5,10,15,20,25)

tags <- c("[0-5]","[5-10]","[10-15]","[15-20]","[21-25]")

group_tags <- cut(air$Wind, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)

summary(group_tags)

##creation of Ozone and Wind boxplot
ggplot(data = air, mapping = aes(x = Ozone, y = Wind)) +
  geom_jitter(aes(color = 'blue'), alpha = 0.2) +
  geom_boxplot(fill = "bisque", color = "black", alpha = 0.3) +
  labs(x = 'Ozone and Wind Boxplot') +
  guides(color = FALSE) +
  theme_minimal()

##Step 4: Explore how the data changes over time
newdates <- paste(air$Month, air$Day, "1973", sep = "/", collapse = NULL)
newdates <- as.Date(newdates, "%m/%d/%y")
newdates

air$Day <- newdates
air

meltedair <- melt(data = air,id.vars = "Day",measure.vars = c("Ozone", "Solar.R","Wind","Temp","Month"))

ggplot(data = meltedair, aes(x = Day, y = value, color = variable)) +
  geom_line()

##Step 5: Look at all the data via a heatmap
ggplot(data = meltedair, aes(x = Day, y = variable)) +
  geom_tile(aes(fill = value))

##Step 6: Scatter plot of Data
air
ggplot(air, aes(x = Wind, y = Temp)) +
  geom_point(aes(colour = factor(Month)))

##Do you see any patterns in the Data?

##If I take a look at the line-chart I can see that the higher the Solar levels 
##the higher the Ozone levels, and they were particualry high between the time of 
##July and August as well as august and September.

##What was the most useful visualization?

##I think the most useful visualization was the Line chart,
##i feel that line charts are very descriptive and can tell a story very easily. 
## 
