library(ggplot2)

##MTcars data set

mtcars
str(mtcars)

##1.Histogram Of MPG:
hist(mtcars$mpg)

##2.Boxplots of mpg by cyl (i.e. 3 box plots, one for all cars with 4 cylinders, 
##one for all cars with 6 cylinders, and one with all the cars with 8 cylinders). 
ggplot(mtcars, aes(x = factor(cyl), mpg))+
  geom_boxplot(outlier.color = "blue", outlier.fill = "red")

##3.MultiLine chart of wt on the x-axis, 
##mpg for the y-axis. With a line for each am 
##(i.e. two lines). Also be sure to show each point on the chart.
q <- ggplot(mtcars, aes(x=factor(wt), y = mpg, color = am, group = am))+
  geom_line()+
  geom_point()
q
##4.Barchart with the x-axis being the name of each car, 
##and the height being wt. Make sure to rotate the x-axis labels, 
##so we can actually read the car name.  
mtcars
carnames <- rownames(mtcars)
carnames

ggplot(mtcars, aes(x=carnames, y=wt)) +
  geom_bar(stat = "identity")
 




mtcars
rownames(mtcars)
carnames <- rownames(mtcars)
barplot(height = mtcars$wt) 
barplot(height = mtcars$wt,names.arg = carnames)
 




##5.Scatter chart with the x-axis being the mpg and the y-axis being the wt of the car. 
##Have the color and the size of each "sybol" (i.e., circle) 
##represent how fast the car goes (based on the qsec attribute).
c <- ggplot(mtcars, aes(x=mtcars$mpg, y=mtcars$wt, label = carnames))
c <- c + geom_point(aes(size = mtcars$qsec, color = mtcars$qsec))
c <- c + geom_text(label = carnames)
c
