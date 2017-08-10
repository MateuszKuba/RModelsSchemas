######## XG BOOST ######## CLASSIFICATION

### LIBRARIES ###
library(xgboost)
library(caret)
library(randomForest)
library(mlr)
library(data.table)
library()
data(PimaIndiansDiabetes)

### DATASET ###
PID <- PimaIndiansDiabetes

### SAMPLE ###
set.seed(3)
train <- createDataPartition(PID$pregnant, p = .8, list = FALSE, times = 1)
training <- PID[train,]
testing <- PID[-train,]

target <- training[,9]
ts_target <- testing[,9]

training <- training[,-9]
testing <- testing[,-9]

### CHECKING AFTER SPLIT  ###
# Original Data
table(PID$diabetes)/nrow(PID)  


# Training Data
table(training$diabetes)/nrow(training)  


# Testing Data#
table(testing$diabetes)/nrow(testing) 

### Convert Factor output to numeric ###

target <- as.numeric(target)-1
ts_target <- as.numeric(ts_target)-1

### Convert data.table ###

setDT(training)
setDT(testing)
training <- as.matrix(training)
testing <- as.matrix(testing)

#preparing matrix
dtrain <- xgb.DMatrix(data = training,label=target)
dtest <- xgb.DMatrix(data = testing,label=ts_target)


#default parameters
params <- list(
  booster = "gbtree",
  #booster = "gblinear",
  objective = "binary:logistic",
  eta=1,
  gamma=0,
  max_depth=2,
  min_child_weight=1,
  subsample=1,
  colsample_bytree=1
)

### xgb cv ###

xgbcv <- xgb.cv(params = params
                ,data = dtrain
                ,nrounds = 3
                ,nfold = 5
                ,showsd = T
                ,stratified = T
                ,print.every.n = 5
                ,early.stop.round = 20
                ,maximize = F
)

xgbcv$best_iteration

#first default - model training
xgb1 <- xgb.train(
  params = params
  ,data = dtrain
  ,nrounds = 2
  ,watchlist = list(val=dtest,train=dtrain)
  ,print_every_n = 1
  ,early_stop_round = 10
  ,maximize = F
  ,eval_metric = "error"
)


#model prediction
xgbpred <- predict(xgb1,dtest)
xgbpred <- ifelse(xgbpred > 0.5,1,0)

### Confusion Matrix ###

library(caret)
confusionMatrix(xgbpred, ts_target)

### XGB Importance ###
mat <- xgb.importance (feature_names = colnames(training),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 
xgb.plot.tree(model = xgb1)