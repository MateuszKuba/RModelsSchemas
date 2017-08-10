######## Decision Tree ########

### LIBRARIES ###
library(mlbench)
library(tree)
library(caret)
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

### MODELLING and CV ###
our.big.tree <- tree(diabetes ~ ., data=training)
cv.results <- cv.tree(our.big.tree, FUN=prune.misclass)

### PLOT BEST TREE SIZE ###
plot(cv.results$size, cv.results$dev, type="b")

### USE ANOTHER VISUALISATION ###
#By default, rpart will conduct as many splits as possible, then use 10–fold cross–validation to prune the tree. #
library(rpart)
library(party)
library(partykit)
rpart1 <- rpart(diabetes ~ ., data = training,
                control = rpart.control(maxdepth = 3))

rpart1a <- as.party(rpart1)


rpartFull <- rpart(diabetes ~ ., data = training)
rpart1a <- as.party(rpartFull)

imp <- (varImp(rpart1, scale = FALSE))
plot(rpart1a)



