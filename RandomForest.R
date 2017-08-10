######## Random Forest ######## CLASSIFICATION

### LIBRARIES ###
library(caret)
library(randomForest)
data(PimaIndiansDiabetes)

### DATASET ###
PID <- PimaIndiansDiabetes

### SAMPLE ###
set.seed(3)
train <- createDataPartition(PID$pregnant, p = .8, list = FALSE, times = 1)
training <- PID[train,]
testing <- PID[-train,]

### CHECKING AFTER SPLIT  ###
# Original Data
table(PID$diabetes)/nrow(PID)  


# Training Data
table(training$diabetes)/nrow(training)  


# Testing Data#
table(testing$diabetes)/nrow(testing)  



### Fit Random Forest Model ###
rf = randomForest(diabetes ~ .,  
                  ntree = 100,
                  data = training)
plot(rf)

#Method 2 caret #
model_rf <- caret::train(diabetes ~ .,
                         data = training,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  savePredictions = TRUE, 
                                                  verboseIter = FALSE))

### Variable Importance ###
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")

### Predict ###
predicted <- predict(model_rf , testing)
predicted2 <- predict(rf , testing)

## Confusion Matrix ###
confusionMatrix(testing[,9],predicted)
confusionMatrix(testing[,9],predicted2)

## Number of variables chosen at each split
#mtry is no of Variables randomly chosen at each split

### REGRESSION RANDOM FOREST ###

oob.err=double(13)
test.err=double(13)
for(mtry in 1:13) 
{
  rf=randomForest(diabetes ~ . , data = training ,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$err.rate[400] #Error of all Trees fitted
  
  pred<-predict(rf,PID[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(PID[-train,], mean( (diabetes - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}
