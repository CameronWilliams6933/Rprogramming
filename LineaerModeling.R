library(rvest)
library(readxl)
library(ggplot2)

data <- read_excel("mlr01.xls")
data
str(data)
colnames(data)
cnames <- colnames(data)
cnames[1] <- "springFcount"
cnames
colnames(data) <- cnames
data
cnames <- colnames(data)
cnames[2] <- "Sizeofadultpop"
cnames
colnames(data) <- cnames
data
cnames <- colnames(data)
cnames[3] <- "Perc.in"
colnames(data) <- cnames
data
cnames <- colnames(data)
cnames[4] <- "WinterSeverity"
cnames
colnames(data) <- cnames
data
##==============================================
plot(data$Sizeofadultpop, data$springFcount)
plot(data$Perc.in, data$springFcount)
plot(data$WinterSeverity, data$springFcount)
## y = SpringFawnCount and x = SizeOfAdult
S <- ggplot(data, aes(x = Sizeofadultpop, y = springFcount)) 
S <- S + geom_point(aes(size = WinterSeverity))
S
##==============================================
## y = springFcount and x = Perc.in
M <- ggplot(data, aes(x = Perc.in, y = springFcount))
M <- M + geom_point(aes(colour  = factor(Sizeofadultpop)))
M
##==============================================
## y = springFcount and x = Perc.in
C <- ggplot(data, aes(x = WinterSeverity, y = springFcount))
C <- C + geom_point(aes(colour = factor(WinterSeverity), size = Sizeofadultpop))
C
##=============================================
model1 <- lm(formula = springFcount ~ WinterSeverity, data = data)
plot(data$WinterSeverity, data$springFcount)
abline(model1)
summary(model1)

model2 <- lm(forumla = springFcount ~ WinterSeverity + Sizeofadultpop, data = data)
summary(model2)

model3 <- lm(formula = springFcount ~ WinterSeverity + Sizeofadultpop + Perc.in, data = data)
summary(model3)

mod <- lm(formula = springFcount ~ data$Sizeofadultpop, data = data)
summary(mod)
##==================================================
## which model Works best ? 
## Both the model2 and model3 work best due to the fact that their P-value is low 
## and can be considered statistically significant, the R- sqaured value of both also are both 97% 
## which means that the variables that are independant can almost perfectely predict the dependant variable. 

##Which of the predictors are statistically significant in each model?
## in model1 the WinterSeverity P value is 0.36263 which makes in fairly close to 
## 0.05 and not maintaining great statistical signifiance. In Model2, the most statisticially significant coefficient
## would have to be the Pericipitation variable seeing that its 0.0217 which is also the same in the 
## third model. 

##If you wanted to create the most parsimonious model (i.e., the one that did the best job with the fewest predictors), 
##what would it contain?
## Since the sizeofadultpop has the lowest p value of 0.0005471 as well as a
## 88% R - squared value when paired with the dependent value of springFcount it is the greatest pairing that offers
## the best job with the fewest amount of variables

