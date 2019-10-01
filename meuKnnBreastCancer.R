rm(list = ls())   #clean workspace
cat("\014")       #clean console

#Librarys####
library(caret) #Library needed for normalization
library(class) #Library needed for KNN

setwd("D:\\puc\\DM\\Exercícios") #Setting directory
cancer = read.csv("breastCancer.csv") #reading database

#Database analys####
head(cancer)
summary(cancer)
str(cancer)

#Column ID isn't usefull for solve the problem
cancer$id <- NULL
str(cancer)


#Split to train and test####
indexes <- sample(1:nrow(cancer), size = 0.3*(nrow(cancer)))
train = cancer[-indexes,]
test = cancer[indexes,]

#Normalization####
modelNorm <- preProcess(train, method = "range")
#Train DB
trainNorm <- predict(modelNorm, train)
str(trainNorm)
#Test DB
testNorm <- predict(modelNorm, test)

#Training the model####
#For this model is needed remove the label, because of this I wrote trainNorm[,-1],testNorm[,-1]
model_knn <- knn(trainNorm[,-1],testNorm[,-1],cl = trainNorm$diagnosis, k =5) #K is the number of neighbors

#Prediction Performance####
cm = table(model_knn, testNorm$diagnosis)
cm
accuracy = 1 - mean(model_knn != testNorm$diagnosis)
print('Accuracy: ')
print(accuracy)
kappa = confusionMatrix(cm)$overall[2]
print('Kappa: ')
print(kappa)
