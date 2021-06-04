#get the data and view it 
data("mtcars")
View(mtcars)
#To get the information about the type of data, 
#print the head values (top 6 values) and analyze. 
head(mtcars)
#print the summary of the dataset to gain knowledge about it. 
summary(mtcars)
#-------------------------------------------------------------------------------
# Sub task 1
#-------------------------------------------------------------------------------
# Linear Regression 
linearRegression<-lm(mpg~am, data=mtcars)
lr<- summary(linearRegression)
lr
# Equation of Line of Regression
cat("Equation of Line of Regression: 
    mpg = (", round(lr$coefficients[2],2), ") am + (", round(lr$coefficients[1],2), ")")
#plot graph
plot( mtcars$am,mtcars$mpg, main = "am vs mpg",
      xlab = "am", ylab = "mpg",cex=0.1, pch = 19, frame = FALSE)
#draw the Line of regression
#method 1
abline(linearRegression, col = "blue")
#method 2
#lines(mtcars$am, predict(linearRegression))

#Error in our Linear regression model
cat("Error in our Linear regression model: ",
    round(100*sigma(linearRegression)/ mean(mtcars$mpg),4), "%")
#-------------------------------------------------
# Multiple Regression 
multipleRegression<-lm(mpg ~ am + wt + hp, data=mtcars)
multipleRegression
mr<- summary(multipleRegression)
mr
coef<- mr$coefficients
# Equation of Line of Regression
cat("Equation of Line of Regression: 
    mpg = (", 
    round(coef[2],2), ") am + (", 
    round(coef[3],2), ") wt + (", 
    round(coef[4],2), ") hp + (", 
    round(coef[1],2), ")
    ")

#plot graph
plot(multipleRegression)

plot(data$mpg, 2.08*data$am - 2.88*data$wt - 0.4*data$hp + 34)

#Error in our Multiple regression model
cat("Error in our Multiple regression model: ",
    round(100*sigma(multipleRegression)/ mean(mtcars$mpg),4), "%")
#-------------------------------------------------
# Curvilinear Regression 
# 1- Implementation for degree 2. Also called Quadratic Regression 
data <- data.frame(x= mtcars$wt,
                   y= mtcars$mpg)
# add a column for x^2
data$x2<- data$x^2 
#view data 
data


#fit quadratic regression model
quadraticModel <- lm(y ~ x+x2, data=data)

#view model summary
qm<- summary(quadraticModel)
qm

#find equation of LOR
coef<- qm$coefficients
cat("After solving the above equations, we get the equation of Line of Regression as:
    (", coef[1],") + (",coef[2],")x + (",coef[3],")x^2")

xValues <- seq(0, 60, 0.1)
yPredict <- predict(quadraticModel,list(x=xValues, x2=xValues^2))

#plot graph
plot(data$x, data$y,
     xlab="wt", ylab="mpg", pch=16)
#draw LOR
lines(xValues, yPredict, col='blue')

#Error in our Quadratic regression model
cat("Error in our Quadratic regression model: ",
    round(100*sigma(quadraticModel)/ mean(mtcars$mpg),4), "%")

#~~~~~~~~~~~~~
# 2- For any degree (z) of curvilinear regression
x<- mtcars$hp
y<- mtcars$mpg

# taking the initial values of the linear regression model. 
# Thus, z=1, a= lr$$coefficients[1], b= lr$coefficients[2]
curvilinear <- nls(y ~ a + x^z, start = list(a= 1,z= 1))
#The above line gives error 
# Thus use nlsLM for better fit. 

install.packages("minpack.lm")
library(minpack.lm)

curvilinear <- nls(y ~ a + b * x, start = list(a= 1, b=1))

# x1<- x^2
# curvilinear <- nls(y ~ a + b * x1, start = list(a= 1, b=1))

#To reduce minFactor under control in nls, it is better to first check that the 
#model is appropriate for the data and that starting values are reasonable. 
# curvilinear <- nlsLM(y ~ a + b * I(x^z), start = list(a= lr$coefficients[1],b= lr$coefficients[2],z=1), 
#                   control=nls.control(maxiter = 500, minFactor = 1/2048))
cr<- summary(curvilinear)
cr

#Error in our Curvilinear regression model
cat("Error in our Curvilinear regression model: ",
    round(100*sigma(curvilinear)/ mean(mtcars$mpg),4), "%")

#plot graph
plot(data$y ~ data$x)
#draw line of regression
lines(data$x, fitted(curvilinear), lty = 2, col = "blue", lwd = 2)

#-------------------------------------------------
# Power Curve Regression
x<- mtcars$hp
y<- mtcars$mpg

powerCurve <- lm(log(y) ~ log(x))
powerCurve

#coefficients
coef<- coefficients(powerCurve)

# power formula: y = a*(x^b)
pcurve <- nls(y ~ a*(x^b),start = list(a = exp(coef[1]), b = coef[2])) 
pc<- summary(pcurve)
pc

#equation of LOR
coef<- pc$coefficients

cat("The equation for Line of Regression is:
    mpg = (", coef[1],") * [ hp ^ (",coef[2],")]")

#plot graph
plot(y ~ x, xlab="hp", ylab="mpg", main= "hp vs mpg", pch=16)
#draw line of regression
curve(predict(pcurve, newdata = data.frame(x)), add=T)

#Error in our Power curve regression model
cat("Error in our Power Curve regression model: ",
    round(100*sigma(pcurve)/ mean(mtcars$mpg),4), "%")

#-------------------------------------------------
# Ridge Regression
install.packages("glmnet")
library(glmnet)

#Analysis 1- Taking different values of lambda, and analysing the plot.
x <- mtcars[, 2:11]
y <- data$y

grid = 10^seq(10, -2, length = 100)
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid)
#print summary
summary(ridge_mod)
#plot graph
plot(ridge_mod) 

# ~~~~~~~~~~
#Analysis 2- Taking a fixed value of lambda (lambda=1), and analysing the plot.
ridge_mod = glmnet(x, y, alpha = 0, lambda = 1)
ridge_mod
summary(ridge_mod)

#Error in our Ridge curve regression model
cat("Error in our Ridge regression model: ",
    round(100*sigma(ridge_mod)/ mean(mtcars$mpg),4), "%")

#predicting dependent variable
dfridge= mtcars[order(mtcars$hp),]
row.names(dfridge)<- NULL
Mridge= data.matrix((dfridge[, 2:11]))
head(Mridge)

dfridge$predicted<- predict(ridge_mod, s=0.1 , newx = Mridge)

#plot graph
plot(mtcars$hp, mtcars$mpg)
#draw LOR
lines(dfridge$hp, dfridge$predicted, col= "Blue")
#-------------------------------------------------
# Lasso Regression
lasso_mod = glmnet(x, y, alpha = 1, lambda = grid)
summary(lasso_mod)

#plot graph
plot(lasso_mod) 

# ~~~~~~~~~~
#Analysis 2- Taking a fixed value of lambda (lambda=1), and analysing the plot.
lasso_mod = glmnet(x, y, alpha = 1, lambda = 1)
lasso_mod
summary(lasso_mod)

#Error in our lasso curve regression model
cat("Error in our lasso regression model: ",
    round(100*sigma(lasso_mod)/ mean(mtcars$mpg),4), "%")

#predicting dependent variable
dflasso= mtcars[order(mtcars$hp),]
row.names(dflasso)<- NULL
Mlasso= data.matrix((dflasso[, 2:11]))
head(Mlasso)

dflasso$predicted<- predict(lasso_mod, s=0.1 , newx = Mlasso)

#plot graph
plot(mtcars$hp, mtcars$mpg)
#draw LOR
lines(dflasso$hp, dflasso$predicted, col= "Red")
#------------
#Comparing the outputs of ridge and lasso regression.
plot(mtcars$hp, mtcars$mpg)
lines(dfridge$hp, dfridge$predicted, col= "Blue")
lines(dflasso$hp, dflasso$predicted, col= "Red")
#-------------------------------------------------------------------------------
# Sub task 2- Gradient Descent
#-------------------------------------------------------------------------------
# Building the model
x<- mtcars$am
y<- mtcars$mpg

# squared error cost function	
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

alpha <- 0.1 #learning rate
num_iters <- 1000 #number of iterations
cost_history <- rep(0,num_iters)
theta_history <- list(num_iters)
theta <-  c(0,0) # Initial values of theta i.e. (intercept, slope)= (0,0)

X <- cbind(1,x)  # Add a column vector with all values to be 1 to x so that 
                 # hypothesis function has an intercept 

for (i in 1:num_iters) {
   theta[1] <- theta[1] - alpha * (1/length(y)) * sum(((X%*%theta)- y))
   theta[2] <- theta[2] - alpha * (1/length(y)) * sum(((X%*%theta)- y)*X[,2])
   cost_history[i] <- cost(X, y, theta)
   theta_history[[i]] <- theta
 } 

theta

# Equation of Line of Regression
cat("Equation of Line of Regression: 
    mpg = (", round(theta[2],2), ") am + (", round(theta[1],2), ")")

#plot graph
plot( mtcars$am,mtcars$mpg, main = "am vs mpg",
     xlab = "am", ylab = "mpg",cex=0.1, pch = 19, frame = FALSE)
abline(theta, col= "Blue")



