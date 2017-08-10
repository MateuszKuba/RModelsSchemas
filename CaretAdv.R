######## XG BOOST ######## CLASSIFICATION
library(profvis)
### LIBRARIES ###

library(mlbench)
library(caret)
library(e1071)
library(ranger)
library(nnet)
library(xgboost)
library(plyr)
library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations


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


### MODELLING ### CARET

control = trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 3,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary)


glm.model= caret::train(diabetes ~ .,
                  data = training,
                  method = "glm",
                  metric = "ROC",
                  trControl = control)

svm.model= caret::train(diabetes ~ .,
                 data = training,
                 method = "svmRadial",
                 metric = "ROC",
                 trControl = control)

rpart.model= caret::train(diabetes ~ .,
                 data = training,
                 method = "rpart",
                 metric = "ROC",
                 trControl = control)

xgbtree.model= caret::train(diabetes ~ .,
                   data = training,
                   method = "xgbTree",
                   metric = "ROC",
                   trControl = control)

avnnet.model= caret::train(diabetes ~ .,
                     data = training,
                     method = "avNNet",
                     metric = "ROC",
                     trControl = control)

nnet.model= caret::train(diabetes ~ .,
                     data = training,
                     method = "nnet",
                     metric = "ROC",
                     trControl = control)

#Random Forest #
randomforest.model= caret::train(diabetes ~ .,
                  data = training,
                  method = "ranger",
                  metric = "ROC",
                  trControl = control)


############################################### UNBALANCED MODELLING #######################################################


test_roc <- function(model, data) {  ## dopasowane do danych PID z diabetes prognozowana i pos / neg jako factors
  
  roc(testing$diabetes,predict(model, testing, type = "prob")[,"neg"])
  
}


### Increasing performance for imbalanced class ###

randomforest.model %>%
  test_roc(data = testing) %>%
  auc()


####################### WEIGHTED MODEL ################

# Create model weights (they sum to one)

model_weights <- ifelse(training$diabetes == "neg",
                        (1/table(training$diabetes)[1]) * 0.5,  ##1 / liczbaNegatywnych *0.5
                        (1/table(training$diabetes)[2]) * 0.5)  ## 1 / liczbaPosytywnych *0.5


randomforest.model.weight= caret::train(diabetes ~ .,
                                 data = training,
                                 method = "ranger",
                                 metric = "ROC",
                                 trControl = control,
                                 weights = model_weights)

######################## UP SAMPLE #####################

## modify trainControl ##
control$sampling <- "up"

randomforest.model.up = caret::train(diabetes ~ .,
                                 data = training,
                                 method = "ranger",
                                 metric = "ROC",
                                 trControl = control)

######################## DOWN SAMPLE #################

## modify trainControl ##
control$sampling <- "down"

randomforest.model.down = caret::train(diabetes ~ .,
                                 data = training,
                                 method = "ranger",
                                 metric = "ROC",
                                 trControl = control)



# Examine results for test set

model_list <- list(original = randomforest.model,
                   weighted = randomforest.model.weight,
                   down = randomforest.model.down,
                   up = randomforest.model.up,
                   neuralNet = nnet.model,
                   avneuralNet = avnnet.model,
                   rPart = rpart.model,
                   svm = svm.model,
                   glm = glm.model
                   )

model_list_roc <- model_list %>%
  map(test_roc, data = testing)

model_list_roc %>%
  map(auc)


######################### PLOT ROC ###############

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
 # scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

########################### RESAMPLING ##################################

resamps <- resamples(list(original = randomforest.model,
                   weighted = randomforest.model.weight,
                   down = randomforest.model.down,
                   up = randomforest.model.up,
                   neuralNet = nnet.model,
                   avneuralNet = avnnet.model,
                   rPart = rpart.model,
                   svm = svm.model,
                   glm = glm.model
))

summary(resamps)

dotplot(resamps, metric = "ROC")



################################ IMPUTING VALUE #############################

preProcValues <- preProcess(PID, method = c("knnImpute"))

library(RANN)

t_imp <- predict(preProcValues, PID)







