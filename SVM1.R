library(kernlab)
library(gridExtra)
library(grid)
library(ggplot2)
library(e1071)
library(dplyr)



##Step1:
airquality$Ozone[is.na(airquality$Ozone)] <- round(mean(airquality$Ozone, na.rm = TRUE))
airquality$Solar.R[is.na(airquality$Solar.R)] <- round(mean(airquality$Solar.R, na.rm = TRUE))

##Step 2: Create Trained data and test data
nrows <- nrow(airquality)
nrows

cutpoint <- floor(nrows/3*2)
cutpoint

rand <- sample(1:nrows)
head(rand)

AQ.trained <- airquality[rand[1:cutpoint],]
AQ.test <- airquality[rand[(cutpoint+1):nrows],]

##Step 3: building the model
model <- ksvm(Ozone~., data = AQ.trained)
model

pred <- predict(model, AQ.test)
plot1 <- plot(AQ.trained,pred, col = "blue", pch = 4)
lm1 <- lm(airquality$Ozone ~ airquality$Solar.R)
plot(lm1)


##table(pred, AQ.test$Ozone)
nrow(pred)

dim(airquality)
dim(pred)
errorOzone <- airquality[1:51,1]

## Computing Root Mean Squared Error
error <- errorOzone - pred

## create a scatter plot
newAQ <- airquality[1:51,1:6]


gg1 <- ggplot(newAQ, aes(Temp,Wind)) +
  geom_point(aes(color = error, size = error))
gg1

##grid.arrange(gg1, lm1, plot1)  
##the grid.arrange function would not work for me

##SVM model
svmfit <- svm(Ozone~., data = AQ.trained, kernel = "linear", cost = .1, scale = FALSE)
print(svmfit)
##For some reason this plot did not work and i do not know why
plot(svmfit, AQ.trained)
##add grid of plots
mean(airquality$Ozone)


##Step 4: Creating goodOzone variable
##42.09804
airquality$goodOzone <- as.numeric(airquality$Ozone >= 42)

nrows <- nrow(airquality)
nrows

cutpoint <- floor(nrows/3*2)
cutpoint

rand <- sample(1:nrows)
head(rand)

AQ.trained <- airquality[rand[1:cutpoint],]
AQ.test <- airquality[rand[(cutpoint+1):nrows],]

model1_2 <- ksvm(goodOzone~., data = AQ.trained)
model1_2

pred1 <- predict(model1_2,AQ.test)
pred1 <- pred1 * 100
pred1
## creating table and the number that was correct
comptable <- data.frame(AQ.test[,7], pred1[,1])


correct <- sum(with(comptable, AQ.test...7. == 1))

number_of_rows <- nrow(comptable)
number_of_rows

correctPred <- correct/number_of_rows *100
correctPred

## determine if prediction is correct or wrong
comptable$correct_or_wrong <- as.character(comptable$AQ.test...7. >= 1 )
comptable

##Step 5: 

plot_kvsm <- data.frame(comptable$correct_or_wrong, airquality$Temp, airquality$Wind, airquality$goodOzone, pred1)
plot_kvsm$pred1 <- as.numeric(plot_kvsm$pred1 >= 42)
colnames(plot_kvsm) <- c("correct", "Temp", "Wind", "goodOzone", "Predict")




ggplot(plot_kvsm, aes(x = Temp, y = Wind)) +
      geom_point(aes(shape = factor(Predict), 
                     colour = factor(goodOzone), 
                     size = factor(correct)))

## using SVM
model_svm <- svm(goodOzone~., data = AQ.trained)
pred <- predict(model_svm, AQ.trained)      
plot(AQ.trained, pch = 16)
plot(AQ.trained, pred, col = "red", pch = 4)
 
##using NaiveBayes
model_nb <- naiveBayes(goodOzone ~., data = plot_kvsm)
model_nb

## Step 6: Which are the best Models for this data?

## This assignment was used to teach me how to create prediction models as well as
## how to plot them and interpret them. The model that I feel was the best was the plot_ksvm model, it was a model that was
## very discriptive and had the lowest training error amount and it gave me confidence that the model had some 
## relevance. One of my plots for some reason did not work and I couldnt figure out why. The grid arrange function was not
## working for me nor was I able to plot the naive bayes model but it allowed me to see all y - corridnates that
## that it predicted. 

