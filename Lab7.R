#get the data and view it 
data("mtcars")
View(mtcars)
#To get the information about the type of data, 
#print the head values (top 6 values) and analyze. 
head(mtcars)
#print the summary of the dataset to gain knowledge about it. 
summary(mtcars)
#-------------------------------------------------------------------------------
# Sub task 1- logistic regression using glm() function
#-------------------------------------------------------------------------------
#install.packages("caret")
library(caret) 

#install.packages("caTools")
library(caTools)

# Splitting data into train and test data 
set.seed(123)
split <- sample.split(mtcars, SplitRatio = 0.7) 
train <- subset(mtcars, split == TRUE) 
test <- subset(mtcars, split == FALSE) 

#print head of training set 
head(train)

#print head of testing set
head(test)

#checking which attribute is well suited for the logistic regression model
fit <- glm(formula = am ~ cyl + hp + wt
           ,data = train
           ,family = 'binomial')
summary(fit)

#we observe that wt has the p value less than the alpha value.
#thus, selecting the attribute wt and y= am
fit1 <- glm(formula = am ~ wt
           ,data = train
           ,family = 'binomial')
summary(fit1)

#----
#plotting the curve
wtRange = seq(min(mtcars$wt),max(mtcars$wt),0.01)
predicted <- predict(fit1, list(wt= wtRange), type = "response")

plot(mtcars$wt, mtcars$am)
lines(wtRange, predicted)

#plotting linear regression line also to compare both the curves
linearRegression<-lm(am~wt, data=mtcars)
abline(linearRegression, col="blue")

#----
#Evaluating the model
#print the dimensions of the training and testing set. 
dim(train)
dim(test)

#predicting values for test data
predictY <- predict(fit, test)
predictY

#converting to probability as the glm() function gives logit 
logit_prob <- function(logit) {
  odds <- exp(logit)
  prob <- odds/(1 + odds)
  return (prob)
}
predictY <- logit_prob(predictY)
predictY

#converting to classifications
#if prob > 0.5, predicted y = 1 
#else predicted y = 0

predictY <- ifelse(predictY > 0.5, 2, 1)
predictY

str(predictY)

testY = test$am
str(testY)

#evaluating the model
confuseMat <- table(predictY, testY+1) #adding 1 to testY as its value is 0 & 1, 
                                       #and that of predictY is 1 & 2.

#printing the confusion matrix
confuseMat

#printing the confusion matrix and statistics
confusionMatrix(confuseMat)

#-------------------------------------------------------------------------------
# Sub task 2- logistic regression using own function
#-------------------------------------------------------------------------------
