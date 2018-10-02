#Justin Tse
#Random Forest
rm(list=ls())
#Read in Data
library(randomForest)
library(data.table)
library(randomForest)
dt <- fread("lag2Data10percentSample.csv",nrows = -1)
dt <- data.frame(dt)
set.seed(1)

####################################################################
#Random Forests 
#First change the response variables to factors 
for(i in 76:97){
  dt[,i] <- as.factor(dt[,i])
}
test <- dt[1:9137,]
train <- dt[9138:91367,]
names(dt)[76:97]
?randomForest
errors <- data.frame(one = c(0,0,0,0,0,0))
for(i in 76:97){
  errVec <- c(0,0,0,0,0,0)
  #Create the formula
  n1 <- names(train)[c(1:75)] 
  f1 <- as.formula(paste(paste(names(train)[i],"~"), paste(n1[!n1 %in% "match"], collapse = " + "))) 
  rf=randomForest(f1 , data = train , subset = 1:82230,ntree = 20, mtry = 16)
  plot(rf)
  #Get Training error
  print(names(train)[i])
  prediction <- predict(rf, train)
  prediction <- as.integer(as.character(prediction))
  #Training Error
  trainErr <- 1- mean(prediction == train[,i])
  cat("general train err", "\t", trainErr,"\n")
  errVec[1] <- trainErr
  
  index <- which(train[,i-22] == 0 & train[,i] == 1)
  newProdTrainErr <- 1-mean(prediction[index] == train[index,i])
  cat("new product train err", "\t", newProdTrainErr,"\n")
  errVec[2] <- newProdTrainErr
  
  index <- which(train[,i-22] == 1 & train[,i] == 0)
  dropProdTrainErr <- 1-mean(prediction[index] == train[index,i])
  cat("drop product train err", "\t", dropProdTrainErr,"\n")
  errVec[3] <- dropProdTrainErr
  
  #Test Error
  testPrediction <- as.integer(as.character(predict(rf, test)))
  testErr <- 1- mean(testPrediction == test[,i])
  cat("general test err", "\t", testErr,"\n")
  errVec[4] <- testErr
  
  index <- which(test[,i-22] == 0 & test[,i] == 1)
  newProdTestErr <- 1-mean(testPrediction[index] == test[index,i])
  cat("new product test err", "\t", newProdTestErr,"\n")
  errVec[5] <- newProdTestErr
  
  index <- which(test[,i-22] == 1 & test[,i] == 0)
  dropProdTestErr <- 1- mean(testPrediction[index] == test[index,i])
  cat("drop product test err", "\t", dropProdTestErr,"\n")
  errVec[6] <- dropProdTestErr
  errors <- data.frame(errors,errVec)
}
t(errors[1,])
errors$one <- NULL
cat(mean(t(errors)[,1], na.rm = T))
cat(mean(t(errors)[,2], na.rm = T))
cat(mean(t(errors)[,3], na.rm = T))
cat(mean(t(errors)[,4], na.rm = T))
cat(mean(t(errors)[,5], na.rm = T))
cat(mean(t(errors)[,6], na.rm = T))
for(i in 1:6){
  cat(mean(t(errors)[,i], na.rm = T),"\n")
}

#EXPERIMENT 2
errors <- data.frame(c(0,0,0,0,0,0))
totTrained <- 0
for(i in c(93:97)){
  indexa <- which(train[,i-22] == 0 & train[,i] == 1)
  indexb <- which(train[,i-22] == 1 & train[,i] == 0)
  indexc <- which(train[,i-22] == train[,i])
  indexd <- c(indexa, indexb, indexc[1:(length(indexa)+length(indexb))])
  #print(length(indexd))
  errVec <- c(0,0,0,0,0,0)
  print(names(train)[i])
  #Create the formula
  n1 <- names(train)[c(1:75)] 
  f1 <- as.formula(paste(paste(names(train)[i],"~"), paste(n1[!n1 %in% "match"], collapse = " + "))) 
  rf=randomForest(f1 , data = train[indexd,] , subset = 1:length(indexd),ntree = 20)
  
  prediction <- predict(rf, train)
  prediction <- as.integer(as.character(prediction))
  #Training Error
  trainErr <- 1- mean(prediction == train[,i])
  cat("general train err", "\t", trainErr,"\n")
  errVec[1] <- trainErr * length(indexd)
  #New Products Error
  #get indices of ppl who added products
  index <- which(train[,i-22] == 0 & train[,i] == 1)
  newProdTrainErr <- 1-mean(prediction[index] == train[index,i])
  cat("new product train err", "\t", newProdTrainErr,"\n")
  errVec[2] <- newProdTrainErr * length(indexd)
  #Drop products error
  #get indices of ppl who dropped products
  index <- which(train[,i-22] == 1 & train[,i] == 0)
  dropProdTrainErr <- 1-mean(prediction[index] == train[index,i])
  cat("drop product train err", "\t", dropProdTrainErr,"\n")
  errVec[3] <- dropProdTrainErr * length(indexd)
  #Test Error
  testPrediction <- as.integer(as.character(predict(rf, test)))
  testErr <- 1- mean(testPrediction == test[,i])
  cat("general test err", "\t", testErr,"\n")
  errVec[4] <- testErr * length(indexd)
  #New Products Error
  #get indices of ppl who added products
  index <- which(test[,i-22] == 0 & test[,i] == 1)
  newProdTestErr <- 1-mean(testPrediction[index] == test[index,i])
  cat("new product test err", "\t", newProdTestErr,"\n")
  errVec[5] <- newProdTestErr * length(indexd)
  #Drop products error
  #get indices of ppl who dropped products
  index <- which(test[,i-22] == 1 & test[,i] == 0)
  dropProdTestErr <- 1- mean(testPrediction[index] == test[index,i])
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
