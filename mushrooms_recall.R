rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console

#Setar pasta de trabalho
setwd("D:\\Manoela\\Dropbox\\Aulas\\Data Mining\\Aula 06 - Classificação\\Enviar")

#Carregar modelo
load("forest_model")

#base a ser inferida
recall = read.table("mushrooms_sem_rotulo.txt", header = TRUE)
inferencia = predict(forest_model, recall)
View(inferencia)

#Juntar base e inferencia
baseComInferencia = cbind(recall, inferencia)
View(baseComInferencia)

