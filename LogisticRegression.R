######## GLM LOGISTIC REGRESSION ########

### LIBRARIES ###
library(ROCR)
library(mlbench)
library(boot)
library(caret)
data(PimaIndiansDiabetes)

### DATASET ###
PID <- PimaIndiansDiabetes

### SAMPLE ###
set.seed(3)
train <- createDataPartition(PID$glucose, p = 0.8, list = FALSE, times = 1)
training <- PID[train,]
testing <- PID[-train,]

### CHECKING AFTER SPLIT  ###
# Original Data
table(PID$diabetes)/nrow(PID)  


# Training Data
table(training$diabetes)/nrow(training)  


# Testing Data#
table(testing$diabetes)/nrow(testing) 

### PREPROCESS ###
preObj <- c("center","scale","pca")

### MODELLING ###
model <- glm(diabetes ~ ., data=training, family=binomial(logit))
#or#
model <- train(training$diabetes ~ .,method="glm",preProcess=preObj,data=training)

summary(model)

### CV GLM ###
cv <- cv.glm(PID,model,K=10)

### PREDICT >0.5 ###
predictions <- predict(model, type="response", newdata=testing)

### ROC ###
pr <- prediction(predictions,testing$diabetes)
prf <- performance(pr, measure = c("tpr"), x.measure = "fpr")
plot(prf,col=rainbow(10))


### CONVERT PROB INTO CLASS
predictions <- round(predictions) # ROC 0.5
predictions <- ifelse(predictions == 1, "pos", "neg")

### CONFUSION MATRIX ###
confusionMatrix(predictions, testing[,9]) 

