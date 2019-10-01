rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console

# Instala pacotes (Comentar após a primeira utilização)
#install.packages("e1071")           # Support Vector Machine (SVM)
#install.packages("tree")            # Decision Tree
#install.packages("randomForest")    # Random Forest

# Carrega pacotes
library("e1071")  
library("tree")
library(randomForest)
library(ggplot2)
# Prepara o ambiente e carrega os dados do exercício
setwd("D:\\puc\\DM")
Credito = read.csv("VRA_20196.csv", header = TRUE)
View(Credito)

summary(Credito)
str(Credito)

d <- subset(Credito, )
Credito$CLASSE <- as.factor(Credito$CLASSE)

index <- sample(1:nrow(Credito), size = 0.3*nrow(Credito))
train = Credito[-index,]
test = Credito[index,]

summary(Credito)

#svm
svm_model <- svm(CLASSE ~., train, probability=T)
predSVM <- predict(svm_model, test, probability = T)
table(predSVM, test$CLASSE)
acuracy = 1 - mean(predSVM != test$CLASSE)
acuracy
summary(svm_model)
