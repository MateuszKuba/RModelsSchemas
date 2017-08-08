######## K-Nearest Neighbours ############## CLASSIFICATION


### LIBRARIES AND DATASETS ###
library(class)
library(mlbench)
library(chemometrics)
library(caret)
data(PimaIndiansDiabetes)

### DATASET ###
PID <- PimaIndiansDiabetes

### SAMPLE ###
set.seed(3)
train <- createDataPartition(PID, p = .8, list = FALSE, times = 1)
training <- PID[train,]
testing <- PID[-train,]

### ERROR EVALUATION FOR DIFFERENT K ###
resknn <- knnEval(scale(PID[,-9]), PID[,9], train, kfold=10,
                 knnvec=seq(1,50,by=1),
                 legpos="bottomright")

### PREDICT ###
predictions <- knn(scale(training[,-9]),
               scale(testing[,-9]),
               training[,9], k=27)


### CONFUSION MATRIX ###

confusionMatrix(testing[,9],predictions)

