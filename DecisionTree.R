######## Decision Tree ########

### LIBRARIES ###
library(mlbench)
library(tree)
data(PimaIndiansDiabetes)

### DATASET ###
PID <- PimaIndiansDiabetes

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
plot(rpart1a)

rpartFull <- rpart(Class ~ ., data = training)
rpart1a <- as.party(rpartFull)
plot(rpart1a)

#https://www.r-project.org/conferences/useR-2013/Tutorials/kuhn/user_caret_2up.pdf