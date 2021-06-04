#Q1------------------------------------------------------------------------------------
#import dataset
flight_fare <- read.csv("C:/Users/sonis/Desktop/CSE 6 Sem/2- lab/1- DA/flight_fare.csv")
View(flight_fare)


#Q2------------------------------------------------------------------------------------
#subsetting 
subs<- flight_fare[, c(1,2,3,6,7)]
head(subs) 
#checking for NA values
is.na(subs)
#finding number of NA values present
sum(is.na(subs))


#Q3------------------------------------------------------------------------------------
#statistical measures
#mean 
mean(flight_fare $ Price)
#median
median(flight_fare $ Price)
#standard deviation 
sd(flight_fare $ Price)
#summary
summary(flight_fare)



#Q4------------------------------------------------------------------------------------
#First converting categorical to numeric 
#METHOD 1- using for loop
#converting Airline to numeric data
for(i in 1:99){
  if(flight_fare$Airline[i]== "IndiGo")
    flight_fare$Airline[i] <- 1
  else if(flight_fare$Airline[i]=="Air India")
    flight_fare$Airline[i]<- 2
  else if(flight_fare$Airline[i]=="Jet Airways")
    flight_fare$Airline[i]<- 3
  else if(flight_fare$Airline[i]=="SpiceJet")
    flight_fare$Airline[i]<- 4
  else if(flight_fare$Airline[i]=="Multiple carriers")
    flight_fare$Airline[i]<- 5
  else if(flight_fare$Airline[i]=="GoAir")
    flight_fare$Airline[i]<- 6
  else if(flight_fare$Airline[i]=="Vistara")
    flight_fare$Airline[i]<- 7
  else if(flight_fare$Airline[i]=="Air Asia")
    flight_fare$Airline[i]<- 8
  else
      flight_fare$Airline[i]<- 0
}

#METHOD 2- using match() function
#converting Source to numeric data
flight_fare$Source<- match(flight_fare$Source, unique(flight_fare$Source))

#converting Destination to numeric data
flight_fare$Destination<- match(flight_fare$Destination, unique(flight_fare$Destination))

#view the modified dataset
View(flight_fare)

#          _________________
#Part 1- scatter plots for the dependent variable wrt all independent variables.
#1.	Price vs Airline 
a<- flight_fare$Price
b<- as.numeric(flight_fare$Airline)
plot( a,b, main = "Price vs Airline",
      xlab = "Price", ylab = "Airline",cex=0.1, pch = 19, frame = FALSE)
abline(lm(b ~ a, data = flight_fare), col = "blue")

#2.	Price vs Source
b<- as.numeric(flight_fare$Source)
plot( a,b, main = "Price vs Source",
      xlab = "Price", ylab = "Source",cex=0.1, pch = 19, frame = FALSE)
abline(lm(b ~ a, data = flight_fare), col = "red")

#3.	Price vs Destination
b<- as.numeric(flight_fare$Destination)
plot( a,b, main = "Price vs Destination",
      xlab = "Price", ylab = "Destination",cex=0.1, pch = 19, frame = FALSE)
abline(lm(b ~ a, data = flight_fare), col = "red")

#4.	Price vs Duration (in hours)
b<- flight_fare$Duration..in.hours.
plot( a,b, main = "Price vs Duration (in hours)",
      xlab = "Price", ylab = "Duration (in hours)",cex=0.1, pch = 19, frame = FALSE)
abline(lm(b ~ a, data = flight_fare), col = "blue")

#5.	Price vs Duration (in mins)
b<- flight_fare$Duration..in.mins.
plot( a,b, main = "Price vs Duration (in mins)",
      xlab = "Price", ylab = "Duration (in mins)",cex=0.1, pch = 19, frame = FALSE)
abline(lm(b ~ a, data = flight_fare), col = "blue")

#6.	Price vs Total_Stops 
b<- flight_fare$Total_Stops
plot( a,b, main = "Price vs Total_Stops",
      xlab = "Price", ylab = "Total_Stops",cex=0.1, pch = 19, frame = FALSE)
abline(lm(b ~ a, data = flight_fare), col = "red")


#          _________________
#Part 2- correlation coefficient
x<- array(c(as.numeric(flight_fare$Airline),as.numeric(flight_fare$Source),
            as.numeric(flight_fare$Destination),flight_fare$Duration..in.hours., 
            flight_fare$Duration..in.mins., flight_fare$Total_Stops, 
            flight_fare$Price), 
          dim=c(99,7))
x
cor(x)
#storing the values of correlation coefficient in variable y . (upto 2 decimal places)
y<- round(cor(x), digits=2)
y


#          _________________
#part 3- regression line
   #done with part 1


#          _________________
#part 4- line by using quadratic and exponential regression equations.
#quadratic
p<- flight_fare$Duration..in.hours.
q <- p^2
r<- flight_fare$Price

quadratic.model <-lm(r ~ p + q)

pqvalues <- seq(0, 300000, 0.1)
predictedr <- predict(quadratic.model,list(p=pqvalues, q=pqvalues^2))
plot(p, r,pch=16, xlab = "Duration(in hours)", ylab = "Price", 
     cex.lab = 0.1, col = "blue")

#Now we include the quadratic model to the plot using the lines() command.
lines(pqvalues, predictedr, col = "green", lwd = 2)


#--
#exponential
s<- flight_fare$Duration..in.hours.
t<- flight_fare$Price
exponential.model <- lm(log(t)~ s)
svalues <- seq(0, 30, 0.1)
t.exponential2 <- exp(predict(exponential.model,list(s=svalues)))
plot(s, t,pch=16)

#Now we include the exponential model to the plot using the lines() command.
lines(svalues, t.exponential2,lwd=2, cex.lab = 0.1,col = "pink", 
      xlab = "Duration(in hours)", ylab = "Price")



#Q5------------------------------------------------------------------------------------
#outlier detection
boxplot.stats(flight_fare $ Price)$out

#to get the row number of the outlier value 
out <- boxplot.stats(flight_fare $ Price)$out
out_ind <- which(flight_fare $ Price %in% c(out))
out_ind

