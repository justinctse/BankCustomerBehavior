#Justin Tse
#Milestone 3 Random Forests
rm(list=ls())
#Read in the data
dt <- read.csv("milestone3data.csv")
#I currently track activity in Month 10, for the model I should not know this
dt$ind_actividad_cliente.1 <- NULL
#Convert response variable to factor for random forests
dt$finActivity <- as.factor(dt$finActivity)
#Split Data into test and training set
set.seed(112)
randInd <- sample(nrow(dt),as.integer(nrow(dt)*.1))
test <-  dt[(randInd), ]
train <- dt[-(randInd),]
#Check the proportions of the response variable in each set, Ideally we will want 5.5% in both the train and test set
mean(dt$finActivity == 0)
mean(test$finActivity == 0)
mean(train$finActivity == 0)

#Function to calculate the different types of errors 
#Takes in a random forest object, test, and train
accRF <- function(rf,test,train){
  errors <- c(0,0,0,0)
  #Get the training prediction
  trainPred <- predict(rf,train)
  trainPred <- as.integer(as.character(trainPred))
  #Get the general training error
  errors[1] <- mean(trainPred == train[,1])
  #Get the training error when identyifying 0s
  #Out of all the 0s how many did we correctly identify 
  indof0TR <- which(train[,1] == 0)
  errors[2] <- mean(trainPred[indof0TR] == train[,1][indof0TR]) 
  #Get the test prediction
  testPred <- predict(rf,test)
  testPred <- as.integer(as.character(testPred))
  #Get the general test error
  errors[3] <- mean(testPred == test[,1])
  #Get the test error when identifying 0s
  indof0Te <- which(test[,1] == 0)
  errors[4] <- mean(testPred[indof0Te] == test[,1][indof0Te])
  names(errors) <- c("genTrainAcc", "0TrainAcc", "genTestAcc", "0TestAcc")
  return(errors)
}
#Run random forest with class rebalancing on the train set
#The ratios used are j:1 corresponding to 0 cases:1 cases
#Return the indices to use
classRebalance <- function(x, test, train){
  indof0 <- which(train[,1] == 0)
  indof1 <- which(train[,1] == 1)
  return(c(indof0,indof1[1:(x*length(indof0))]))
}
#############################################

#Perform the random forests algorithm
library(randomForest)
#rf=randomForest(finActivity ~ ., data = train , subset = 1:nrow(train),ntree = 50)
rf=randomForest(finActivity ~ ., data = train ,ntree = 15)
plot(rf)
rf
varImpPlot(rf)
accRF(rf,test,train)
#It seems that random forests DOESN'T have better performance than a SINGLE decision tree
#Could be too much homogeneity in the data, I will get rid of the month 5 data to check this
names(train)[57:78]
rf=randomForest(finActivity ~ ., data = train[,1:56] ,ntree = 15)
plot(rf)
varImpPlot(rf)
accRF(rf,test,train) #Dropping the last month of behaviour doesn't change much




rf = randomForest(finActivity ~ ., data = train[classRebalance(1,test,train),1:56] ,ntree = 15)
classRebalanceAccuracyDT <- t(data.frame(accRF(rf,test,train[classRebalance(1,test,train),])))
for(j in 2:10){
  rf = randomForest(finActivity ~ ., data = train[classRebalance(j,test,train),1:56] ,ntree = 15)
  classRebalanceAccuracyDT <- rbind(classRebalanceAccuracyDT,accRF(rf,test,train[classRebalance(j,test,train),]))
}
classRebalanceAccuracyDT


rf = randomForest(finActivity ~ ., data = train[classRebalance(9,test,train),] ,ntree = 25)
accRF(rf,test,train)
?randomForest
rf = randomForest(finActivity ~ ., data = train[classRebalance(9,test,train),1:56] ,ntree = 25)
plot(rf)
varImpPlot(rf)
accRF(rf,test,train)
names(train)[1:34]
#NO raw BEHAVIOUR DATA 
rf = randomForest(finActivity ~ ., data = train[classRebalance(9,test,train),1:34] ,ntree = 25)
plot(rf)
varImpPlot(rf)
accRF(rf,test,train)
#9:1 ratio
#ntree = 25, all behavioural data 0.7894614   0.8093399   0.7879994   0.7995724
#ntree = 25, last month behavioural 0.7794337   0.8114466   0.7780038   0.8086585 
#ntree = 25 NO raw behavioural 0.7891612   0.7899696   0.7881447   0.7936932 

#Parameter Tuning
rf = randomForest(finActivity ~ ., data = train[classRebalance(9,test,train),1:56] ,ntree = 50, mtry = 9)

plot(rf)
rf
varImpPlot(rf)
accRF(rf,test,train)
rf

#Graph the VIP in ggplot
library(ggplot2)
library(scales)
rfVI <- data.frame(rf$importance, var = rownames(rf$importance))
rfVI <- rfVI[order(rfVI$MeanDecreaseGini, decreasing = F),]
rfVI$var <- factor(rfVI$var, levels = rfVI$var) #This keeps the columns in the right order 
ggplot(rfVI, aes(var,MeanDecreaseGini))+geom_bar(stat = "identity", fill = "#A2071A") + 
  ylab("Mean Decrease Gini Index") + xlab("Feature") + ggtitle("sdf") +coord_flip()


rfVI <- data.frame(rf$importance, var = rownames(rf$importance))
rfVI <- rfVI[order(rfVI$MeanDecreaseGini, decreasing = T),]
rfVI$var <- factor(rfVI$var, levels = rfVI$var) #This keeps the columns in the right order 
ggplot(rfVI, aes(var,MeanDecreaseGini))+geom_bar(stat = "identity", fill = "#A2071A") + 
  ylab("Mean Decrease Gini Index") + xlab("Feature") + ggtitle("sdf") + 
  coord_cartesian(ylim = c(250, max(rfVI$MeanDecreaseGini*1.001)))
  