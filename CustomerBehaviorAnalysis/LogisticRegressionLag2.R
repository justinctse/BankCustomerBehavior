#Justin Tse
#LOGISTIC REGRESSION
rm(list=ls())
#Read in Data
library(data.table)
#dt <- fread("lag2Data10percentSample.csv",nrows = -1)
dt <- fread("lag2Data.csv",nrows = -1)
dt <- data.frame(dt)
names(dt)[76:97]
#The response variables are 141:162
#response <- dt[,141:162]
#dt[,141:162]<-NULL

#Specify the three types of errors we want to test
#General Accuracy refers to everyone

#New Products, referes to the people who added products
names(dt)[c(54,76)]
nrow(dt[dt[,54] == 0 & dt[,76] == 1,]) #Number of people who added a product

#Product Drop, refers to the people who dropped products
nrow(dt[dt[,54] == 1 & dt[,76] == 0,])


#SPLIT DATA INTO TEST AND TRAIN SET
#test <- dt[1:9137,]
#train <- dt[9138:91367,]
set.seed(1)
randInd <- sample(nrow(dt),as.integer(nrow(dt)*.1))
test <-  dt[(randInd), ]
train <- dt[-(randInd),]
#THE LOGISTIC REGRESSION
library(glmnet)
errors <- data.frame(c(0,0,0,0,0,0))
for(i in 1:2){
cat("a","\t","b","\n")
cat("a","\t","b","\n")}

for(i in 76:97){
  errVec <- c(0,0,0,0,0,0)
  print(names(train)[i])
  logReg = glmnet(data.matrix(train[,1:75]), data.matrix(train[,i]), family = "binomial")
  prediction <- predict(logReg, data.matrix(train[,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)])
  prediction <- as.numeric(prediction)
  #Training Error
  trainErr <- 1- mean(prediction == train[,i])
  cat("general train err", "\t", trainErr,"\n")
  errVec[1] <- trainErr
  #New Products Error
  #get indices of ppl who added products
  index <- which(train[,i-22] == 0 & train[,i] == 1)
  newProdTrainErr <- 1-mean(prediction[index] == train[index,i])
  cat("new product train err", "\t", newProdTrainErr,"\n")
  errVec[2] <- newProdTrainErr
  #Drop products error
  #get indices of ppl who dropped products
  index <- which(train[,i-22] == 1 & train[,i] == 0)
  dropProdTrainErr <- 1-mean(prediction[index] == train[index,i])
  cat("drop product train err", "\t", dropProdTrainErr,"\n")
  errVec[3] <- dropProdTrainErr
  #Test Error
  testPrediction <- as.numeric(predict(logReg, data.matrix(test[,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)]))
  testErr <- 1- mean(testPrediction == test[,i])
  cat("general test err", "\t", testErr,"\n")
  errVec[4] <- testErr
  #New Products Error
  #get indices of ppl who added products
  index <- which(test[,i-22] == 0 & test[,i] == 1)
  newProdTestErr <- 1-mean(testPrediction[index] == test[index,i])
  cat("new product test err", "\t", newProdTestErr,"\n")
  errVec[5] <- newProdTestErr
  #Drop products error
  #get indices of ppl who dropped products
  index <- which(test[,i-22] == 1 & test[,i] == 0)
  dropProdTestErr <- 1- mean(testPrediction[index] == test[index,i])
  cat("drop product test err", "\t", dropProdTestErr,"\n")
  errVec[6] <- dropProdTestErr
  errors <- data.frame(errors,errVec)
}

#Test what happens when you restrict data to new product customers and drop product customers
i <- 78
index1 <- which(train[,i-22] == 0 & train[,i] == 1)
index2 <- which(train[,i-22] == 1 & train[,i] == 0)
index <- c(index1, index2)
testReg <- glmnet(data.matrix(train[index,1:75]), data.matrix(train[index,i]), family = "binomial")
prediction <- predict(testReg, data.matrix(train[index,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)])
prediction <- as.numeric(prediction)
#Training error when restricted to this set 
1-mean(prediction == train[index,i])
#General error on "Train" set when we add the other types of customers
prediction <- predict(testReg, data.matrix(train[,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)])
prediction <- as.numeric(prediction)
#Training Error
1- mean(prediction == train[,i])
#New Products Error
#get indices of ppl who added products
index <- which(train[,i-22] == 0 & train[,i] == 1)
newProdTrainErr <- 1-mean(prediction[index] == train[index,i])
print(paste("The new product training error is",newProdTrainErr))
#Drop products error
#get indices of ppl who dropped products
index <- which(train[,i-22] == 1 & train[,i] == 0)
dropProdTrainErr <- 1-mean(prediction[index] == train[index,i])
print(paste("The drop product training error is",dropProdTrainErr))
#Get Test Error
testPrediction <- as.numeric(predict(testReg, data.matrix(test[,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)]))
testErr <- 1- mean(testPrediction == test[,i])
print(paste("The general test error is",testErr))
#New Products Error
#get indices of ppl who added products
index <- which(test[,i-22] == 0 & test[,i] == 1)
newProdTestErr <- 1-mean(testPrediction[index] == test[index,i])
print(paste("The new product test error is",newProdTestErr))
#Drop products error
#get indices of ppl who dropped products
index <- which(test[,i-22] == 1 & test[,i] == 0)
dropProdTestErr <- 1- mean(testPrediction[index] == test[index,i])
print(paste("The drop product training error is",dropProdTestErr))


#RESULTS OF THIS TEST:
#If we restrict to just observations where a customer adds a product or drops a product then we will get perfect
#accuracy on those cases but horrible accuracy on the rest. 
i<-78
#Experiment 2 
#What happens when we add an equal amount of data 
index1 <- which(train[,i-22] == 0 & train[,i] == 1)
index2 <- which(train[,i-22] == 1 & train[,i] == 0)
index3 <- which(train[,i-22] == train[,i])
index <- c(index1, index2, index3[1:701])
testReg <- glmnet(data.matrix(train[index,1:75]), data.matrix(train[index,i]), family = "binomial")
prediction <- predict(testReg, data.matrix(train[index,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)])
prediction <- as.numeric(prediction)
#Training error when restricted to this set 
1-mean(prediction == train[index,i])
#General error on "Train" set when we add the other types of customers
prediction <- predict(testReg, data.matrix(train[,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)])
prediction <- as.numeric(prediction)
#Training Error
1- mean(prediction == train[,i])
#New Products Error
#get indices of ppl who added products
index <- which(train[,i-22] == 0 & train[,i] == 1)
newProdTrainErr <- 1-mean(prediction[index] == train[index,i])
print(paste("The new product training error is",newProdTrainErr))
#Drop products error
#get indices of ppl who dropped products
index <- which(train[,i-22] == 1 & train[,i] == 0)
dropProdTrainErr <- 1-mean(prediction[index] == train[index,i])
print(paste("The drop product training error is",dropProdTrainErr))
#Get Test Error
testPrediction <- as.numeric(predict(testReg, data.matrix(test[,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)]))
testErr <- 1- mean(testPrediction == test[,i])
print(paste("The general test error is",testErr))
#New Products Error
#get indices of ppl who added products
index <- which(test[,i-22] == 0 & test[,i] == 1)
newProdTestErr <- 1-mean(testPrediction[index] == test[index,i])
print(paste("The new product test error is",newProdTestErr))
#Drop products error
#get indices of ppl who dropped products
index <- which(test[,i-22] == 1 & test[,i] == 0)
dropProdTestErr <- 1- mean(testPrediction[index] == test[index,i])
print(paste("The drop product training error is",dropProdTestErr))

#Experiment 3 Num normal customers is half the sum of special 
index1 <- which(train[,i-22] == 0 & train[,i] == 1)
index2 <- which(train[,i-22] == 1 & train[,i] == 0)
index3 <- which(train[,i-22] == train[,i])
index <- c(index1, index2, index3[1:351])
testReg <- glmnet(data.matrix(train[index,1:75]), data.matrix(train[index,i]), family = "binomial")
prediction <- predict(testReg, data.matrix(train[index,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)])
prediction <- as.numeric(prediction)
#Training error when restricted to this set 
1-mean(prediction == train[index,i])
#General error on "Train" set when we add the other types of customers
prediction <- predict(testReg, data.matrix(train[,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)])
prediction <- as.numeric(prediction)
#Training Error
1- mean(prediction == train[,i])
#New Products Error
#get indices of ppl who added products
index <- which(train[,i-22] == 0 & train[,i] == 1)
newProdTrainErr <- 1-mean(prediction[index] == train[index,i])
print(paste("The new product training error is",newProdTrainErr))
#Drop products error
#get indices of ppl who dropped products
index <- which(train[,i-22] == 1 & train[,i] == 0)
dropProdTrainErr <- 1-mean(prediction[index] == train[index,i])
print(paste("The drop product training error is",dropProdTrainErr))
#Get Test Error
testPrediction <- as.numeric(predict(testReg, data.matrix(test[,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)]))
testErr <- 1- mean(testPrediction == test[,i])
print(paste("The general test error is",testErr))
#New Products Error
#get indices of ppl who added products
index <- which(test[,i-22] == 0 & test[,i] == 1)
newProdTestErr <- 1-mean(testPrediction[index] == test[index,i])
print(paste("The new product test error is",newProdTestErr))
#Drop products error
#get indices of ppl who dropped products
index <- which(test[,i-22] == 1 & test[,i] == 0)
dropProdTestErr <- 1- mean(testPrediction[index] == test[index,i])
print(paste("The drop product training error is",dropProdTestErr))

#Repeating experiment 2 with all data
#LOGISTIC REGRESSION
names(dt)[76:97]
#The response variables are 141:162
#response <- dt[,141:162]
#dt[,141:162]<-NULL

#Specify the three types of errors we want to test
#General Accuracy refers to everyone
errors <- data.frame(c(0,0,0,0,0,0))
totTrained <- 0
for(i in c(87:97)){
  indexa <- which(train[,i-22] == 0 & train[,i] == 1)
  indexb <- which(train[,i-22] == 1 & train[,i] == 0)
  indexc <- which(train[,i-22] == train[,i])
  indexd <- c(indexa, indexb, indexc[1:(1*(length(indexa)+length(indexb)))])
  print(length(indexd))
  errVec <- c(0,0,0,0,0,0)
  print(names(train)[i])
  logReg = glmnet(data.matrix(train[indexd,1:75]), data.matrix(train[indexd,i]), family = "binomial")
  prediction <- predict(logReg, data.matrix(train[,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)])
  prediction <- as.numeric(prediction)
  #Training Error
  trainErr <- 1- mean(prediction == train[,i])
  cat("general train err", "\t", trainErr,"\n")
  errVec[1] <- trainErr* length(indexd)
  #New Products Error
  #get indices of ppl who added products
  index <- which(train[,i-22] == 0 & train[,i] == 1)
  newProdTrainErr <- 1-mean(prediction[index] == train[index,i])
  cat("new product train err", "\t", newProdTrainErr,"\n")
  errVec[2] <- newProdTrainErr* length(indexd)
  #Drop products error
  #get indices of ppl who dropped products
  index <- which(train[,i-22] == 1 & train[,i] == 0)
  dropProdTrainErr <- 1-mean(prediction[index] == train[index,i])
  cat("drop product train err", "\t", dropProdTrainErr,"\n")
  errVec[3] <- dropProdTrainErr* length(indexd)
  #Test Error
  testPrediction <- as.numeric(predict(logReg, data.matrix(test[,1:75]), type = "class", s = logReg$lambda[length(logReg$lambda)]))
  testErr <- 1- mean(testPrediction == test[,i])
  cat("general test err", "\t", testErr,"\n")
  errVec[4] <- testErr* length(indexd)
  #New Products Error
  #get indices of ppl who added products
  index <- which(test[,i-22] == 0 & test[,i] == 1)
  newProdTestErr <- 1-mean(testPrediction[index] == test[index,i])
  cat("new product test err", "\t", newProdTestErr,"\n")
  errVec[5] <- newProdTestErr* length(indexd)
  #Drop products error
  #get indices of ppl who dropped products
  index <- which(test[,i-22] == 1 & test[,i] == 0)
  dropProdTestErr <- 1- mean(testPrediction[index] == test[index,i])
  cat("drop product test err", "\t", dropProdTestErr,"\n")
  errVec[6] <- dropProdTestErr* length(indexd)
  errors <- data.frame(errors,errVec)
  totTrained <- length(indexd) + totTrained
}

errors$one <- NULL
#weighted sum
for(i in 1:6){
  cat(sum(t(errors)[,i], na.rm = T)/totTrained,"\n")
}
plot(logReg)
