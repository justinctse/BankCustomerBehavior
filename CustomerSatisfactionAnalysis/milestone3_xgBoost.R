rm(list=ls())
#Read in the data
dt <- read.csv("milestone3data.csv")
#I curretnly track activity in Month 10, for the model I should not know this
dt$ind_actividad_cliente.1 <- NULL

#Split Data into test and training set
set.seed(112)
randInd <- sample(nrow(dt),as.integer(nrow(dt)*.1))
test <-  dt[(randInd), ]
train <- dt[-(randInd),]
test <- data.matrix(test)
train <- data.matrix(train)
#Check the proportions of the response variable in each set, Ideally we will want 5.5% in both the train and test set
mean(dt$finActivity == 0)
mean(test[,1]== 0)
mean(train[,1]== 0)
#######################################
#Function to calculate the different types of errors 
#Takes in an xgb object, test, and train
accXGB <- function(xgb,test,train){
  errors <- c(0,0,0,0)
  #Get the training prediction
  trainPred <- predict(xgb,train)
  trainPred <- trainPred > .5
  #Get the general training error
  errors[1] <- mean(trainPred == train[,1])
  #Get the training error when identyifying 0s
  #Out of all the 0s how many did we correctly identify 
  indof0TR <- which(train[,1] == 0)
  errors[2] <- mean(trainPred[indof0TR] == train[,1][indof0TR]) 
  #Get the test prediction
  testPred <- predict(xgb,test)
  testPred <- testPred > .5
  #Get the general test error
  errors[3] <- mean(testPred == test[,1])
  #Get the test error when identifying 0s
  indof0Te <- which(test[,1] == 0)
  errors[4] <- mean(testPred[indof0Te] == test[,1][indof0Te])
  names(errors) <- c("genTrainAcc", "0TrainAcc", "genTestAcc", "0TestAcc")
  return(errors)
}
#Run xgb with class rebalancing on the train set
#The ratios used are j:1 corresponding to 0 cases:1 cases
#Return the indices to use
classRebalance <- function(x, test, train){
  indof0 <- which(train[,1] == 0)
  indof1 <- which(train[,1] == 1)
  return(c(indof0,indof1[1:(x*length(indof0))]))
}
#####################################


#Perform the XGBoost
#xgb =xgboost(data = train[,2:78], label = train[,1], max_depth = 2, eta = 1, nrounds = 2, min_child_weight = 1,objective = "binary:logistic", verbose = 0)
library(xgboost)
xgb =xgboost(data = train[,2:34], label = train[,1], nrounds = 10, objective = "binary:logistic", verbose = 0)
accXGB(xgb,test,train)

#XGBoost with class rebalancing
ind <- classRebalance(17,test,train)
xgb = xgboost(data = train[ind ,2:34],label = train[ind ,1], nrounds=15, objective = "binary:logistic",verbose = 0)
accXGB(xgb,test,train)

