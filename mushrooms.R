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

# Prepara o ambiente e carrega os dados do exercício
setwd("D:\\puc\\DM")
Mushroom = read.table("mushrooms.txt", header = TRUE)

# Gera aleatoriamente os indices para base de teste (30% para teste)
set.seed(0)
summary(Mushroom)
indexes = sample(1:nrow(Mushroom), size=0.3*nrow(Mushroom))
train = Mushroom[-indexes,]
test = Mushroom[indexes,]


# ===================================================================================================

# SVM
svm_model <- svm(class ~., train, probability =T)
predictionsSVM <- predict(svm_model, test, probability =T)
table(predictionsSVM,test$class)
acuracy = 1 - mean(predictionsSVM != test$class)
acuracy
summary(svm_model)
#cost ou C => custo das violações das restrições (constante de regularização do termo de Lagrange)
#gamma => define a distância de influência dos padrões nos limites de decisão (valores baixos => longe; valores altos => perto)

probabilidades = attr(predictionsSVM, "probabilities")
predictionsAndProbabilities = cbind(test$class, predictionsSVM, probabilidades)
View(predictionsAndProbabilities)

# D-Tree
tree_model <- tree(class ~., train)
predictionsDtree <- predict(tree_model, test, type = "class")
table(predictionsDtree, test$class)
acuracy = 1 - mean(predictionsDtree != test$class)
acuracy
summary(tree_model)
plot(tree_model)
text(tree_model)
  
# Random Forest
system.time(forest_model <- randomForest(class ~., data = train, importance = TRUE, do.trace = 100))
predictionsForest = predict(forest_model, test)
table(predictionsForest, test$class)
acuracy = 1 - mean(predictionsForest != test$class)
acuracy
plot(forest_model)
legend("topright", legend=c("OOB", "0", "1"),
       col=c("black", "red", "green"), lty=1:1, cex=0.8)
#lty = line type, cex = character expansion factor

#Duas medidas de importância para rankear os atributos
varImpPlot(forest_model)


############## RECALL ##################################
#Salvar modelo para utilizá-lo quando chegarem novos dados
save(forest_model, file = 'forest_model')

library(tensorflow)
install_tensorflow(version = "gpu")

