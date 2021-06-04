#import dataset
Insurance <- read.csv("C:/Users/sonis/Desktop/CSE Sem 6/2- lab/1- DA/Insurance.csv")
View(Insurance)

#converting categorical data to numerical data. 
Insurance$sex<- match(Insurance$sex, unique(Insurance$sex))
Insurance$region<- match(Insurance$region, unique(Insurance$region))
Insurance$smoker<-  match(Insurance$smoker, unique(Insurance$smoker))
#view the modified dataset
View(Insurance)

#Q1-distances-----------------------------------------------------------------------------------
#jaccard distance
distance_bin <- function(x, y){
  x <- x != 0
  y <- y != 0
  a <- sum(x & y)
  b <- sum (x | y)
  1 - (a / b)
}

print(paste("Jaccard similarity b/w Age and Charges: ",
            1-round(distance_bin(Insurance$age, Insurance$charges),2)))  #print upto 2 decimal places 
print(paste("Jaccard similarity b/w Sex and Charges: ",
            1-round(distance_bin(Insurance$sex, Insurance$charges),2))) 
print(paste("Jaccard similarity b/w BMI and Charges: ",
            1-round(distance_bin(Insurance$bmi, Insurance$charges),2))) 
print(paste("Jaccard similarity b/w Children and Charges: ",
            1-round(distance_bin(Insurance$children, Insurance$charges),2))) 
print(paste("Jaccard similarity b/w Smoker and Charges: ",
            1-round(distance_bin(Insurance$smoker, Insurance$charges),2))) 
print(paste("Jaccard similarity b/w Region and Charges: ",
            1-round(distance_bin(Insurance$region, Insurance$charges),2))) 



#euclidean
distance_eu<- function(x,y){
  sqrt(sum((abs(x - y))^2 ))
}

print(paste("Euclidean distance b/w Age and Charges: ",
            round(distance_eu(Insurance$age, Insurance$charges),2)))  #print upto 2 decimal places 
print(paste("Euclidean distance b/w Sex and Charges: ",
            round(distance_eu(Insurance$sex, Insurance$charges),2))) 
print(paste("Euclidean distance b/w BMI and Charges: ",
            round(distance_eu(Insurance$bmi, Insurance$charges),2))) 
print(paste("Euclidean distance b/w Children and Charges: ",
            round(distance_eu(Insurance$children, Insurance$charges),2))) 
print(paste("Euclidean distance b/w Smoker and Charges: ",
            round(distance_eu(Insurance$smoker, Insurance$charges),2))) 
print(paste("Euclidean distance b/w Region and Charges: ",
            round(distance_eu(Insurance$region, Insurance$charges),2))) 

#manhattan
distance_man <- function(x,y){
  sum(abs(x-y))
}

print(paste("Manhattan distance b/w Age and Charges: ",
            round(distance_man(Insurance$age, Insurance$charges),2)))  #print upto 2 decimal places 
print(paste("Manhattan distance b/w Sex and Charges: ",
            round(distance_man(Insurance$sex, Insurance$charges),2))) 
print(paste("Manhattan distance b/w BMI and Charges: ",
            round(distance_man(Insurance$bmi, Insurance$charges),2))) 
print(paste("Manhattan distance b/w Children and Charges: ",
            round(distance_man(Insurance$children, Insurance$charges),2))) 
print(paste("Manhattan distance b/w Smoker and Charges: ",
            round(distance_man(Insurance$smoker, Insurance$charges),2))) 
print(paste("Manhattan distance b/w Region and Charges: ",
            round(distance_man(Insurance$region, Insurance$charges),2))) 

#supremum
distance_sup <- function(x,y){
  max(abs(x - y))
}

print(paste("Supremum distance b/w Age and Charges: ",
            round(distance_sup(Insurance$age, Insurance$charges),2)))  #print upto 2 decimal places 
print(paste("Supremum distance b/w Sex and Charges: ",
            round(distance_sup(Insurance$sex, Insurance$charges),2))) 
print(paste("Supremum distance b/w BMI and Charges: ",
            round(distance_sup(Insurance$bmi, Insurance$charges),2))) 
print(paste("Supremum distance b/w Children and Charges: ",
            round(distance_sup(Insurance$children, Insurance$charges),2))) 
print(paste("Supremum distance b/w Smoker and Charges: ",
            round(distance_sup(Insurance$smoker, Insurance$charges),2))) 
print(paste("Supremum distance b/w Region and Charges: ",
            round(distance_sup(Insurance$region, Insurance$charges),2))) 

#minkowski
p <- 3 #let p=3
distance_min <- function(x, y, p){
  (sum((abs(x - y))^p))^(1/p)
}

print(paste("Minkowski distance b/w Age and Charges: ",
            round(distance_min(Insurance$age, Insurance$charges,p),2)))  #print upto 2 decimal places 
print(paste("Minkowski distance b/w Sex and Charges: ",
            round(distance_min(Insurance$sex, Insurance$charges,p),2))) 
print(paste("Minkowski distance b/w BMI and Charges: ",
            round(distance_min(Insurance$bmi, Insurance$charges,p),2))) 
print(paste("Minkowski distance b/w Children and Charges: ",
            round(distance_min(Insurance$children, Insurance$charges,p),2))) 
print(paste("Minkowski distance b/w Smoker and Charges: ",
            round(distance_min(Insurance$smoker, Insurance$charges,p),2))) 
print(paste("Minkowski distance b/w Region and Charges: ",
            round(distance_min(Insurance$region, Insurance$charges,p),2))) 

#Q2-cosine similariy-----------------------------------------------------------------------------------
#install the required package.
install.packages("lsa")
library(lsa)

#calculate Cosine Similarity
print(paste("Cosine similarity b/w Age and Charges: ",
            round(cosine(Insurance$age, Insurance$charges),2)))  #print upto 2 decimal places 
print(paste("Cosine similarity b/w Sex and Charges: ",
            round(cosine(Insurance$sex, Insurance$charges),2))) 
print(paste("Cosine similarity b/w BMI and Charges: ",
            round(cosine(Insurance$bmi, Insurance$charges),2))) 
print(paste("Cosine similarity b/w Children and Charges: ",
            round(cosine(Insurance$children, Insurance$charges),2))) 
print(paste("Cosine similarity b/w Smoker and Charges: ",
            round(cosine(Insurance$smoker, Insurance$charges),2))) 
print(paste("Cosine similarity b/w Region and Charges: ",
            round(cosine(Insurance$region, Insurance$charges),2))) 



