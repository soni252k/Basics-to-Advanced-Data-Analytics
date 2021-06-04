#get the data and view it 
data <- read.csv("C:/Users/sonis/Desktop/CSE Sem 6/2- lab/1- DA/fruit_data_with_colors.csv")
View(data)

#To get the information about the type of data, 
#print the head values (top 6 values) and analyze. 
head(data)

#print the summary of the dataset to gain knowledge about it. 
summary(data)

#dropping 1,3 columns for better classification & easier visualization.
data1=data[,c(2,4,5,6,7)]
head(data1)

#install required packages for performing classification algos 
#using install.packages() and load them using the library() function 

#install.packages("caTools")
library(caTools)

#install.packages("party")
library(party)

#install.packages("e1071") 
library(e1071)

#install.packages("class") 
library(class)

#install.packages("caret")
library(caret) 

#install.packages("rpart")
library(rpart)

#install.packages("rpart.plot") 
library(rpart.plot)

#------------------------

#1- Decision tree

evaluate<- function(model, test, actual,mytype){
  predicted= predict(model, test, type= mytype)
  tab= table(predicted, actual)
  tab
  confusionMatrix(tab)
}


# Splitting data into train and test data 
set.seed(123)
split <- sample.split(data1, SplitRatio = 0.7) 
train <- subset(data1, split == TRUE) 
test <- subset(data1, split == FALSE) 

#print head of training set 
head(train)

#print head of testing set
head(test)

#building the tree
tree<- rpart(train$fruit_name ~ . , data=train, 
             method= 'class', minsplit= 1, minbucket=2, 
             parms=list(split= 'infprmation '))
tree

#plot tree
rpart.plot(tree, type= 5, extra= 100)

#evaluate the tree
evaluate(tree, test = test, actual= test[,1], mytype='class')

#---------------------------------

#2- Naive Bayes
#create model
bayes<- naiveBayes(train$fruit_name~. ,data=train)
bayes

#evaluate the model
evaluate(bayes,test = test,actual = test[,1], mytype = 'class')

#---------------------------------

#3- KNN 
# Feature Scaling 
train_scale <- scale(train[, 2:5]) 
test_scale <- scale(test[, 2:5]) 

#print the head values of scaled train and test set
head(train_scale)
head(test_scale)

#fit knn model in the training set
knn <-  knn(train_scale, test_scale, cl = train$fruit_name, k = 5)

class(knn)

#print the head values of knn model
head(knn)

#print the confusion matrix
confusionMatrix(table(knn,test[,1]))

#---------------------------------

#4- SVM

#changing char to factor 
for(i in c(1:ncol(data1))){
  if(class(data1[[i]]) == class('char'))
    data1[[i]] = factor(data1[[i]])
}

#exploring plots to get the best fit for SVM 
plot(data1$mass, data1$color_score, 
     col = data1$fruit_name)
plot(data1$height, data1$color_score, 
     col = data1$fruit_name)
plot(data1$width, data1$color_score, 
     col = data1$fruit_name)
plot(data1$mass, data1$height, 
     col = data1$fruit_name)
plot(data1$width, data1$height, 
     col = data1$fruit_name)
plot(data1$mass, data1$width, 
     col = data1$fruit_name)

#using this plot
plot(data1$width, data1$color_score, 
     col = data1$fruit_name)

#make a new data frame containing the attributes fruit names, width and color score
data2= data1[,c(1,3,5)]
head(data2)

#setting the seed
set.seed(120)

#split the data into training and testing data
splits = sample.split(data2, SplitRatio = .8)
train = subset(data2, split == TRUE)
test = subset(data2, split == FALSE)

#printing the head values of training and testing data
head(train)
head(test)

plot(train$width, train$color_score, 
     col = train$fruit_name)

#fitting the SVM model into the training set
svmfit <- svm(fruit_name~., data = train)
summary(svmfit)

#plotting the SVM model
plot(svmfit, data = train)

#evaluate the model
evaluate(svmfit, test, test[[1]], mytype = 'class')

#finding the accuracy
accuracy <-  mean(test[[1]] == predict(svmfit, test, type =  'class')) * 100
accuracy

#---------------------------------------------------------

#own function for KNN
#utility function to calculate mode of a vector
df <-  data2

Modes <- function(x) {
  unix <- unique(x)
  tab <- tabulate(match(x, unix))
  unix[tab == max(tab)]
}

customKnn = function(df, test_v, k = 5){
  mean_df = c()
  sd_df = c()
  test_scaled = c()
  pair = data.frame(dist = 0 , label = -1)
  
  for(i in 2 : (ncol(df))) {
    mean_df[i] = mean(df[[i]])
    sd_df[i] = sd(df[[i]])
    test_scaled = (test_v - mean_df[i]) / sd_df[i]
  }
  
  for(i in 1 : nrow(df)){
    dist = sqrt(sum((test_scaled - df[i, -1])**2))
    pair[i,] = data.frame(dist,df[i,ncol(df)])
  }
  
  sorted_pair = pair[order(pair[1]),]
  k_sorted_pair = sorted_pair[c(1:k),]
  label = Modes(k_sorted_pair[,2])
  label
}

cat("Accuracy = ", customKnn(df,df[53,-1])*100)

#---------------------------------

