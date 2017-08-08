######## GLM LOGISTIC REGRESSION ########

### LIBRARIES ###
library(ROCR)
library(mlbench)
library(boot)
data(PimaIndiansDiabetes)

### DATASET ###
PID <- PimaIndiansDiabetes

### SAMPLE ###
set.seed(3)
train <- createDataPartition(PID, p = .8, list = FALSE, times = 1)
training <- PID[train,]
testing <- PID[-train,]

### MODELLING ###
model <- glm(diabetes ~ ., data=training, family=binomial(logit))
summary(model)

### CV GLM ###
cv <- cv.glm(PID,model,K=5)

### PREDICT >0.5 ###
predictions <- predict(model, type="response", newdata=testing)

### ROC ###
pr <- prediction(predictions,testing$diabetes)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

### CONVERT PROB INTO CLASS
predictions <- round(predictions)
predictions <- ifelse(predictions == 1, "pos", "neg")

### CONFUSION MATRIX ###
confusionMatrix(predictions, testing[,9]) 

