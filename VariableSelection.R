### DATASET ###
library(factoextra)
library(caret)
pca <- prcomp(PID[,-9], scale = TRUE)


### VISUALIZE PCA ###

fviz_screeplot(pca, addlabels=TRUE)
fviz_pca_ind(pca, geom = "point", habillage = PID[,9], addEllipses = TRUE, ellipse.level= 0.9) + theme_minimal()

### VAR IMPORTANCE ###

### Linear Correlation 
target <- as.numeric(PID[,9])-1
target <-as.data.frame(cor(PID[,-9],y=target))
target.sort <- target[order(target[,"V1"]),  , drop = FALSE]
target.sort

highlyCorrelated = findCorrelation(cor(PID[,-9]), cutoff=0.2)
plot(highlyCorrelated)

