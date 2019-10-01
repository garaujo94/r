rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console

setwd("D:\\puc\\DM\\Exercícios")
ads = read.csv("Social_Network_Ads.csv")
head(ads)
str(ads)
ads$Purchased <- as.factor(ads$Purchased)
ads$Gender <- NULL

summary(ads)

indexes <- sample(1:nrow(ads), size = 0.3*(nrow(ads)))
train <- ads[-indexes,]
test <- ads[indexes,]

model_logReg <- glm(Purchased ~., data = train, family = binomial)
pred <- predict(model_logReg, test)
y_pred = ifelse(pred>0.5,1,0)

table(y_pred, test$Purchased) 

accuracy = 1 - mean(y_pred != test$Purchased)
accuracy

summary(model_logReg)

#Random Forest
system.time(forest_model <- randomForest(Purchased ~., data = train, ntree=10000, importance = TRUE, do.trace = 100))
predictionsForest = predict(forest_model, test)
table(predictionsForest, test$Purchased)
acuracy = 1 - mean(predictionsForest != test$Purchased)
acuracy
plot(forest_model)
legend("topright", legend=c("OOB", "0", "1"),
       col=c("black", "red", "green"), lty=1:1, cex=0.8)
#lty = line type, cex = character expansion factor
