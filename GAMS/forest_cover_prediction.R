
library(caret)
library(randomForest)
library(ranger)
library(e1071)
library(ggplot2)
library(gridExtra)
library(corrplot)

# Forest cover prediction -------------------------------------------------

# Some plots --------------------------------------------------------------
data_forest <- read.csv(file = "train.csv", header=TRUE, sep=",")
summary(data_forest)

forest <- data_forest
forest$Id<- NULL
soil<- forest[ ,c(15:54)]
area<- forest[,c(11:14)]
forest<- forest[,c(-15:-54, -11:-14)]
Newfactor <- factor(apply(soil, 1, function(x) which(x == 1)), labels = c(1:38)) 
forest$Soil_Type<- as.integer(Newfactor)
Newfactor2 <- factor(apply(area, 1, function(x) which(x == 1)), labels = c(1:4)) 
forest$Wilderness_Area<- as.integer(Newfactor2)
forest<- forest[ ,c(1:10,12,13,11)]
head(forest)
forestTrain<-forest

boxplot(forest[,c(-7,-8,-9,-11,-12,-13)], las=3, par(mar = c(15, 4, 2, 2)), col="darkseagreen4",main="General")
theme_set(theme_gray(base_size = 20))


g1<- ggplot(forest, aes(Elevation, color = factor(Cover_Type), fill = factor(Cover_Type))) + geom_density(alpha = 0.2)
g2<- ggplot(forest, aes(Aspect, color = factor(Cover_Type), fill = factor(Cover_Type))) + geom_density(alpha = 0.2)
g3<- ggplot(forest, aes(Horizontal_Distance_To_Roadways, color = factor(Cover_Type), fill = factor(Cover_Type))) + geom_density(alpha = 0.2)
g4<- ggplot(forest, aes(Horizontal_Distance_To_Fire_Points, color = factor(Cover_Type), fill = factor(Cover_Type))) + geom_density(alpha = 0.2)
grid.arrange(g1, g2,g3,g4, ncol=2,nrow=2)

cor<- forest[,c(-9,-8,-7,-13)]
names(cor)<- c("Elevation", "Aspect","Slope","H_D_To_Hydro","V_D_To_Hydro","H_D_To_Roads", "H_D_To_Fire_Points" ,"Soil_Type","Wilderness_Area" )
#Correlation between variables
m<- cor(cor)
corrplot(m, method = "circle", tl.cex=1.2, mar = c(2, 2, 2, 2))



names = colnames(data_forest)[12:56]

#Scale variables
for( i in 1:12){
  data_forest[,i] = scale(data_forest[,i])
}
for( i in 12:55){
  data_forest[,i]  =as.factor(data_forest[,i])
}


#create train and test sets
library(caret)

set.seed(107)
inTrain = createDataPartition(y = data_forest$Cover_Type,
                              p = .75,
                              list = FALSE)
head(inTrain)
training = data_forest[ inTrain, ]
testing = data_forest[-inTrain, ]




# SVM ---------------------------------------------------------------------

svmfit=svm(as.factor(Cover_Type)~ ., data = training, kernel="radial", cost=10,scale=FALSE, cross = 10)
yhat.rf = predict(svmfit,newdata=testing[,-56])

#Miss classification error
round( mean( yhat.rf != testing$Cover_Type )*100, 3)

# Confusion matrix
confusionMatrix(yhat.rf,testing$Cover_Type,mode = "prec_recall")

svm_tune <- tune(svm, as.factor(Cover_Type)~ ., data = training, kernel="radial", ranges=list(cost=2^(-1:5)
                                                                                              , gamma=c(.5,1,2)))

bestmod=svm_tune$best.model
summary(bestmod)
bestmod$cost
bestmod$kernel
bestmod$degree
bestmod$gamma





# Something with Ranger ---------------------------------------------------
#Ranger is a fast implementation of Random Forest (Breiman 2001) or recursive partitioning, 
#particularly suited for high dimensional data. Classification, regression, and survival 
#forests are supported. Classification and regression forests are implemented as in the 
#original Random Forest (Breiman 2001), survival forests as in Random Survival Forests (Ishwaran et al. 2008).

library(ranger)
rang3 <- ranger(as.factor(Cover_Type) ~ .,
                write.forest=TRUE,
                data=training, num.trees = 2000, importance = 'impurity', classification = T) 

# Summary of Ranger object
summary(rang3)

#Cross validation 
rang3.rf.tune = csrf(
  as.factor(Cover_Type) ~ .,
  training_data = training,
  test_data = training,
  params1 = list(num.trees = 1000, mtry=4),
  params2 = list(num.trees = 500, mtry=8)
)



#Train set predictions
yhat.tr <- rang3$predictions
print(yhat.tr)

#Variable importance 
var_imp <- rang3$variable.importance
plot(var_imp, type = 'h')

# Predictions on Test set
yhat.rf.ranger= predict(rang3,data=testing[,-56])

#Miss classification error on Test set
round( mean( yhat.rf.ranger$predictions != testing$Cover_Type )*100, 3)

#Confusion matrix , precision, recall, F-1 score measures

confusionMatrix(yhat.rf.ranger$predictions,testing$Cover_Type, mode = "prec_recall")


