#Justin Tse
#LOGISTIC REGRESSION
rm(list=ls())
#Read in Data
library(data.table)
#dt <- fread("lag5Data10percentSample.csv",nrows = -1)
dt <- fread("lag5Data.csv",nrows = -1)
dt <- data.frame(dt)
names(dt)[142:163]
#The response variables are 141:162
#response <- dt[,141:162]
#dt[,141:162]<-NULL

#Specify the three types of errors we want to test
#General Accuracy refers to everyone

#New Products, referes to the people who added products
names(dt)[c(122,144)]
nrow(dt[dt[,122] == 0 & dt[,144] == 1,]) #Number of people who added a product

#Product Drop, refers to the people who dropped products
nrow(dt[dt[,122] == 1 & dt[,144] == 0,])


#SPLIT DATA INTO TEST AND TRAIN SET
#test <- dt[1:8938,]
#train <- dt[8939:89338,]
set.seed(1)
randInd <- sample(nrow(dt),as.integer(nrow(dt)*.1))
test <-  dt[(randInd), ]
train <- dt[-(randInd),]
#THE LOGISTIC REGRESSION
library(glmnet)
errors <- data.frame(c(0,0,0,0,0,0))
for(i in 142:163){
  errVec <- c(0,0,0,0,0,0)
  print(names(train)[i])
  logReg = glmnet(data.matrix(train[,1:141]), data.matrix(train[,i]), family = "binomial")
  prediction <- predict(logReg, data.matrix(train[,1:141]), type = "class", s = logReg$lambda[length(logReg$lambda)])
  prediction <- as.numeric(prediction)
  #Training Error
  trainErr <- 1- mean(prediction == train[,i])
  print(paste("The general training error is",trainErr))
  errVec[1] <- trainErr
  #New Products Error
  #get indices of ppl who added products
  index <- which(train[,i-22] == 0 & train[,i] == 1)
  newProdTrainErr <- 1-mean(prediction[index] == train[index,i])
  print(paste("The new product training error is",newProdTrainErr))
  errVec[2] <- newProdTrainErr
  #Drop products error
  #get indices of ppl who dropped products
  index <- which(train[,i-22] == 1 & train[,i] == 0)
  dropProdTrainErr <- 1-mean(prediction[index] == train[index,i])
  print(paste("The drop product training error is",dropProdTrainErr))
  errVec[3] <- dropProdTrainErr
  #Test Error
  testPrediction <- as.numeric(predict(logReg, data.matrix(test[,1:141]), type = "class", s = logReg$lambda[length(logReg$lambda)]))
  testErr <- 1- mean(testPrediction == test[,i])
  print(paste("The general test error is",testErr))
  errVec[4] <- testErr
  #New Products Error
  #get indices of ppl who added products
  index <- which(test[,i-22] == 0 & test[,i] == 1)
  newProdTestErr <- 1-mean(testPrediction[index] == test[index,i])
  print(paste("The new product test error is",newProdTestErr))
  errVec[5] <- newProdTestErr
  #Drop products error
  #get indices of ppl who dropped products
  index <- which(test[,i-22] == 1 & test[,i] == 0)
  dropProdTestErr <- 1- mean(testPrediction[index] == test[index,i])
  errVec[6] <- dropProdTestErr
  print(paste("The drop product training error is",dropProdTestErr))
}
for(i in 1:6){
  cat(mean(t(errors)[,i], na.rm = T),"\n")
}



