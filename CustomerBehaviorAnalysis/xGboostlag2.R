#Justin Tse
#Xgboost
rm(list=ls())
#Read in Data
library(xgboost)
library(data.table)
library(randomForest)
dt <- fread("lag2Data10percentSample.csv",nrows = -1)
dt <- data.frame(dt)
set.seed(1)

####################################################################
#Random Forests 
#First change the response variables to factors 

testDT <- dt[1:9137,]
trainDT <- dt[9138:91367,]
names(dt)[76:97]
#Convert input to matrix
test <- data.matrix(testDT)
train <- data.matrix(trainDT)
?xgboost
#min_child_weight  high vals lead to undexgbitting, 1 is default
#maxdepth 3-10 , default = 6, high value leads too ovexgbitting
errors <- data.frame(one = c(0,0,0,0,0,0))
for(i in 76:97){
  errVec <- c(0,0,0,0,0,0)
  #Create the formula
  #xgb =xgboost(data = data.matrix(train[,1:75]), label = data.matrix(train[,i]), max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
  xgb =xgboost(data = train[,1:75], label = train[,i], max_depth = 2, eta = 1, nrounds = 2, min_child_weight = 1,objective = "binary:logistic", verbose = 0)
  #Get Training error
  print(names(dt)[i])
  prediction <- predict(xgb, train)
  prediction <- prediction > .5
  #Training Error
  trainErr <- 1- mean(prediction == train[,i])
  cat("general train err", "\t", trainErr,"\n")
  errVec[1] <- trainErr
  
  index <- which(trainDT[,i-22] == 0 & trainDT[,i] == 1)
  newProdTrainErr <- 1-mean(prediction[index] == trainDT[index,i])
  cat("new product train err", "\t", newProdTrainErr,"\n")
  errVec[2] <- newProdTrainErr
  
  index <- which(trainDT[,i-22] == 1 & trainDT[,i] == 0)
  dropProdTrainErr <- 1-mean(prediction[index] == trainDT[index,i])
  cat("drop product train err", "\t", dropProdTrainErr,"\n")
  errVec[3] <- dropProdTrainErr
  
  #Test Error
  testPrediction <- predict(xgb, test)>.5
  testErr <- 1- mean(testPrediction == testDT[,i])
  cat("general test err", "\t", testErr,"\n")
  errVec[4] <- testErr
  
  index <- which(testDT[,i-22] == 0 & testDT[,i] == 1)
  newProdTestErr <- 1-mean(testPrediction[index] == testDT[index,i])
  cat("new product test err", "\t", newProdTestErr,"\n")
  errVec[5] <- newProdTestErr
  
  index <- which(testDT[,i-22] == 1 & testDT[,i] == 0)
  dropProdTestErr <- 1- mean(testPrediction[index] == testDT[index,i])
  cat("drop product test err", "\t", dropProdTestErr,"\n")
  errVec[6] <- dropProdTestErr
  errors <- data.frame(errors,errVec)
}
t(errors[1,])
errors$one <- NULL
for(i in 1:6){
  cat(mean(t(errors)[,i], na.rm = T),"\n")
}

#EXPERIMENT 2
errors <- data.frame(c(0,0,0,0,0,0))
totTrained <- 0
for(i in c(76:97)){
  indexa <- which(trainDT[,i-22] == 0 & trainDT[,i] == 1)
  indexb <- which(trainDT[,i-22] == 1 & trainDT[,i] == 0)
  indexc <- which(trainDT[,i-22] == trainDT[,i])
  indexd <- c(indexa, indexb, indexc[1:as.integer(.5*(length(indexa)+length(indexb)))])
  #print(length(indexd))
  errVec <- c(0,0,0,0,0,0)
  print(names(dt[i]))

  xgb =xgboost(data = data.matrix(trainDT[indexd,1:75]), label = data.matrix(trainDT[indexd,i]), max_depth = 5, eta = .7, nrounds = 4, min_child_weight = 1,objective = "binary:logistic", verbose = 0)
  prediction <- predict(xgb, train)
  prediction <- prediction >.5
  #Training Error
  trainErr <- 1- mean(prediction == trainDT[,i])
  cat("general train err", "\t", trainErr,"\n")
  errVec[1] <- trainErr * length(indexd)
  #New Products Error
  #get indices of ppl who added products
  index <- which(trainDT[,i-22] == 0 & trainDT[,i] == 1)
  newProdTrainErr <- 1-mean(prediction[index] == trainDT[index,i])
  cat("new product train err", "\t", newProdTrainErr,"\n")
  errVec[2] <- newProdTrainErr * length(indexd)
  #Drop products error
  #get indices of ppl who dropped products
  index <- which(trainDT[,i-22] == 1 & trainDT[,i] == 0)
  dropProdTrainErr <- 1-mean(prediction[index] == trainDT[index,i])
  cat("drop product train err", "\t", dropProdTrainErr,"\n")
  errVec[3] <- dropProdTrainErr * length(indexd)
  #Test Error
  testPrediction <- predict(xgb, test) > .5
  testErr <- 1- mean(testPrediction == testDT[,i])
  cat("general test err", "\t", testErr,"\n")
  errVec[4] <- testErr * length(indexd)
  #New Products Error
  #get indices of ppl who added products
  index <- which(test[,i-22] == 0 & test[,i] == 1)
  newProdTestErr <- 1-mean(testPrediction[index] == testDT[index,i])
  cat("new product test err", "\t", newProdTestErr,"\n")
  errVec[5] <- newProdTestErr * length(indexd)
  #Drop products error
  #get indices of ppl who dropped products
  index <- which(test[,i-22] == 1 & test[,i] == 0)
  dropProdTestErr <- 1- mean(testPrediction[index] == testDT[index,i])
  cat("drop product test err", "\t", dropProdTestErr,"\n")
  errVec[6] <- dropProdTestErr * length(indexd)
  errors <- data.frame(errors,errVec)
  totTrained <- length(indexd) + totTrained
}
errors$one <- NULL
#weighted sum
for(i in 1:6){
  cat(sum(t(errors)[,i], na.rm = T)/totTrained,"\n")
}

?xgboost

