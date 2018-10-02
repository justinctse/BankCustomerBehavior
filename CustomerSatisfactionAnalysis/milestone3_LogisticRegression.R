#Justin Tse
#Milestone 3 Logistic Regression
rm(list=ls())
library(glmnet)
#Read in the data
dt <- read.csv("milestone3data.csv")
#I curretnly track activity in Month 10, for the model I should not know this
dt$ind_actividad_cliente.1 <- NULL
#Split Data into test and training set
set.seed(112)
randInd <- sample(nrow(dt),as.integer(nrow(dt)*.1))
test <-  dt[(randInd), ]
train <- dt[-(randInd),]
#Check the proportions of the response variable in each set, Ideally we will want 5.5% in both the train and test set
mean(dt$finActivity == 0)
mean(test$finActivity == 0)
mean(train$finActivity == 0)


#Create a function to perform the logistic regression
logReg <- function(test, train){
  #We want to keep track of the general error, and error when identifying the 0s
  #Keep track of this in the test and train set 
  errors <- c(0,0,0,0)
  #Train the model 
  logReg <- glmnet(data.matrix(train[,2:ncol(train)]), data.matrix(train[,1]), family = "binomial")
  #Get the training prediction
  trainPred <- predict(logReg, data.matrix(train[,2:ncol(train)]), type = "class", s = logReg$lambda[length(logReg$lambda)])
  trainPred <- as.numeric(trainPred)
  #Get the general training error
  errors[1] <- mean(trainPred == train[,1])
  #Get the training error when identyifying 0s
  #Out of all the 0s how many did we correctly identify 
  indof0TR <- which(train[,1] == 0)
  errors[2] <- mean(trainPred[indof0TR] == train[,1][indof0TR]) 
  #Get the test prediction
  testPred <- predict(logReg, data.matrix(test[,2:ncol(test)]), type = "class" , s = logReg$lambda[length(logReg$lambda)])
  testPred <- as.numeric(testPred)
  #Get the general test error
  errors[3] <- mean(testPred == test[,1])
  #Get the test error when identifying 0s
  indof0Te <- which(test[,1] == 0)
  errors[4] <- mean(testPred[indof0Te] == test[,1][indof0Te])
  names(errors) <- c("genTrainAcc", "0TrainAcc", "genTestAcc", "0TestAcc")
  return(errors)
}
#Function to run the logistic regression algorithm on a rebalanced training set
#x- the multiplier for rebalancing, the ratio between training data and test data is x:1
classRebalance <- function(x, test, train){
  indof0 <- which(train[,1] == 0)
  indof1 <- which(train[,1] == 1)
  return(logReg(test, train[c(indof0,indof1[1:(x*length(indof0))]),]))
}
#General logistic regression
logReg(test, train)

#Run logistic regression with class balancing on the training set
#The ratios used are j:1 corresponding to 0 cases:1 cases
classRebalanceAccuracyDT <- data.frame(t(classRebalance(1,test,train)))
for(j in 2:17){
  classRebalanceAccuracyDT <- rbind(classRebalanceAccuracyDT,classRebalance(j,test,train))
}
classRebalanceAccuracyDT
#Plot the results as a time series
library(ggplot2)
classRebalanceAccuracyDT$j <- 1:17
#20 is essentially the biggest j we can have before going over the entire dataset. 
logregplot<-ggplot(classRebalanceAccuracyDT, aes(j)) + 
  geom_line(aes(y = genTrainAcc, colour = "General Training Accuracy"),size = 1) + 
  geom_line(aes(y = X0TrainAcc, colour = "Inactive Training Accuracy"),size = 1) + 
  geom_line(aes(y = genTestAcc, colour = "General Testing Accuracy"),size = 1) + 
  geom_line(aes(y = X0TestAcc, colour = "Inactive Testing Accuracy"),size = 1) +
  ylab("Accuracy") + xlab("Ratio of Active to Inactive Users in Training Data") + 
  ggtitle("Accuracy of Logistic Regression Over Class Rebalancing Ratios")+
  theme(legend.position="bottom",legend.direction="vertical")

library(cairoDevice) #Use this to save the plot with anti-aliasing enabled
ggsave(plot = logregplot, "LogReg_ClassRebalancePlot2.png", h = 8, w = 11.5, type = "cairo-png")

#LOOK AT WHAT HAPPENS WHEN I REMOVE SOME TRAINING VARS
#This is a way to look at variable importance
#Baseline LogisticRegression at 8:1 ratio
varImpDT <- t(data.frame(classRebalance(8,test,train)))
#Run logistic regression without the engineered features compare with the default logistic regression
#The engineered features are at indices 31:34
varImpDT <- rbind(varImpDT,classRebalance(8,test[,-c(31:34)],train[,-c(31:34)]))
#Run it without the 5th month behaviour
varImpDT <- rbind(varImpDT,classRebalance(8,test[,-c(57:78)],train[,-c(57:78)]))
#Run it without the 5th month behaviour & 10th month behaviour
varImpDT <- rbind(varImpDT,classRebalance(8,test[,-c(35:78)],train[,-c(35:78)]))
#Run it without any behaviour data
varImpDT <- rbind(varImpDT,classRebalance(8,test[,-c(31:78)],train[,-c(31:78)]))
rownames(varImpDT) <- c("alldata","onlyrawdata","no5thmonthdata","norawdata","nobehaviouraldata")


#RESULTS, We only get a big performance drop when we remove ALL of the behavioural data
#It seems the summary is enough to account for the raw behavioural data

#Create a stacked bar plot to visualize this
varImpDT <- data.frame(varImpDT)
varImpDT <- cbind(c("All Data","No Engineered Features","No Month 5 Raw Data","No Raw Data","No Behavioral Data"),varImpDT)

#Write the results to a csv file
#write.csv(varImpDT, "logRegVarImpData.csv", row.names = T)

colnames(varImpDT)[1] <- "Accuracy"
library(reshape2)
varImpDT$Accuracy <- factor(varImpDT$Accuracy, levels = varImpDT$Accuracy) #This is so the plot will leave the x axis in the same order
meltedVarImp <- melt(varImpDT, id.vars = 'Accuracy')
meltedVarImp$variable <- c(rep("General Training Accuracy",5), rep("Inactive Training Accuracy",5),
                           rep("General Testing Accuracy",5), rep("Inactive Testing Accuracy",5))

varImpLogRegPlot <- ggplot(meltedVarImp, aes(Accuracy, value, fill = Accuracy)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")+
  scale_fill_manual(values=c("#9C9B7A", "#F54F29", "#FF974F","#405952"))+
  xlab("Restriction on Training Data") + ylab("Accuracy")+
  ggtitle("Effect of Behavioral Data on Logistic Regression Model, 8:1 Ratio of Rebalanced Classes")

ggsave(plot = varImpLogRegPlot, "LogReg_VarImpPlot.png", h = 6.5, w = 11.5, type = "cairo-png")


#Remove engineered features when trained on the full data
logReg(test,train)
logReg(test[,-c(31:34)],train[,-c(31:34)]) #removing these features turns into 3% less accuracy on 0TestAcc, and 2.5% less on 0TrainAcc