library(arules)
library(dplyr)
library(arulesViz)


t
##=======================================================================
##Step 1
##Descriptive Stats
##1.Compute the percentage of people that survived.
##table(t$Survived)

## By adding the numbers for "yes" and "no" and 
## and dividing it 711 and multiplying it 100 we can get the percentage of 
## number of survivors 
NumberSurvived <- 711/2201 * 100
NumberSurvived
## 32.3% percent of people suvived on the boat

##2.Compute the percentage of people that were children. 
##table(t$Age)

## By adding the numbers for "child" and "adult"
## together and dividing it 109 and multiply it by 100 we can 
## get the answer of number of children on the boat
NumberChildren <- 109/2201 * 100
NumberChildren
## 4.9% of the population was children

##3. Compute the percentage of people that were female.
##table(t$Sex)

## 470 + 1731 * 100 
NumberOfFemales <- 470/2201 * 100
NumberOfFemales
## 21.3% of the population on the boat were female

##4.Finally, compute the percentage of people that were in first class.
##table(t$Class)

## there are 325 people in 1st class, 325/2201 * 100
NumberOffclass <- 325/2201 * 100
NumberOffclass
## 14.7% of people that were on the boat were first class passengers
##===================================================================================

##Step2
##1.What percentage of children survived? Your answer should be written such as # 13.75% of children survived
##table(t$Age, t$Survived)


childrenSurvived <- 57/711*100
childrenSurvived
## 8% of children survived

##2.What percentage of female survived?
##table(t$Sex, t$Survived)


femaleSurvive <- 344/711*100
femaleSurvive
## 48.4% of the population 

##3.What percentage of first-class people survived?
##table(t$Class, t$Survived)


firstclassSurvive <- 203/711*100
firstclassSurvive
## 28.6% of survivors were first class

##4.What percentage of third-class people survived?
thirdclassSurvive <- 178/711*100
thirdclassSurvive
## 25% of the survivors were 3rd class

##===============================================================================
##step:3
##1.Write a function that returns a new dataframe of people that satisfies the specified criteria of sex, age, 
##class and survived as parameters. 
##I'm giving you the answer for this question:
myfunction <- function(a,b,c,d){
  df1 <- t[t$Class == a,] # filter the data that satisfied the criteria that "Class" = a
  df2 <- df1[df1$Sex == b,] # filter the data that satisfied the criteria that "Sex" = b
  df3 <- df2[df2$Age == c,] # filter the data that satisfied the criteria that "Age" = c
  df4 <- df3[df3$Survived == d,] # filter the data that satisfied the criteria that "Survived" = d
  
  numbery <- t %>% filter(Survived == "Yes" & Sex == b & Age == c) %>% count()
  numbern <- t %>% filter(Survived == "No" & Sex == b & Age == c) %>% count()
  
  if(d == "No")
  {
    return((paste0(b, ":", round((numbern * 100 / (numbery + numbern)),2),"%")))
  }
  
  else if (d == "Yes")
  {
    return ((paste0(b, ":", round((numbery * 100/ (numbery + numbern)),2),"%"))
  }
  
}

## Test data 
myfunction("1st", "Female","Adult", "No")

##Use the function to compare age and third-class male survival rates. People in which category are more likely to survive? 
myfunction("3rd", "Male", "Adult", "No")
myfunction("3rd", "Male", "Child", "No")
##First class was more likely to survive

  ##4. Use the function to compare age and first-class female survival rates. People in which category are more likely to survive? 
myfunction("1st", "Female", "Adult", "No")
myfunction("1st", "Female", "Child", "No")
##First class was more likely to survive

##===============================================================================================================================
##Step4:
ruleset <- apriori(t, parameter = list (support = 0.05, confidence = 0.5))
plot(ruleset)
interesting_rules <- ruleset[quality(ruleset)$lift > 2.5]
inspect(interesting_rules)
##3.Pick the three most interesting and useful rules. 
## if you were a first class female you had a high chance of surviving
## if you were simply a first class passenger you had a high rate of surviving 
## if you were an adult in first class and survived, most likley you were a female

##How does this compare to the descriptive analysis we did on the same dataset?
## It shares the same results, using arules to conduct analysis does make the heaving lifting easier though since it generate the association
## for you.

