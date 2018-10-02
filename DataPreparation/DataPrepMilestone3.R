#Justin Tse
#Data Preparation for Milestone 3
rm(list=ls())

#Data Prep for Milestone 3

#Read in the data
#install.packages("data.table")
library(data.table)
dt <- fread("modifiedTrainingData.csv",nrows = -1)
#dt$V1 <- NULL
########################################################################################
#, keep the Last 10 months of Data
length(unique(dt$fecha_dato))
dates <- unique(dt$fecha_dato)[7:17]
#Restrict data to customers that are present in the first month
dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[1],]$ncodpers) #True if we want to keep these customers
#Started with 13433142 obs, 932441 customers, end with 12485959 obs, 799168 customers
length(unique(dt$ncodpers))
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[1],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[2],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[3],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[4],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[5],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[6],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[7],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[8],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[9],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[10],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[11],]$ncodpers),]
dt$V1 <- NULL
demographics <- dt[dt$fecha_dato == dates[10],][,c(1:13,35,36,38:51)]
demographics <- demographics[order(ncodpers)]

#I STILL NEED TO MAKE SURE THAT ALL VARS ARE CATEGORICAL
#sex v is male
demographics$sexo <- demographics$sexo == "V"
demographics$indresi <- demographics$indresi == "S"
demographics$indext <- demographics$indext == "S"
demographics$indfall <- NULL
unique(demographics$ind_empleado)
indEmpF <- demographics$ind_empleado == "F"
indEmpA <- demographics$ind_empleado == "A"
indEmpN <- demographics$ind_empleado == "N"
indEmpB <- demographics$ind_empleado == "B"
indEmpS <- demographics$ind_empleado == "S"
demographics$ind_empleado <- NULL
demographics <- data.frame(demographics, indEmpF,indEmpA,indEmpN,indEmpB,indEmpS)

colnames(dt)[c(1:13,35,36,38:51)]

#Check how many are inactive to start
nrow(dt[dt$fecha_dato == dates[1] & dt$ind_actividad_cliente == 1,]) #344154
nrow(dt[dt$fecha_dato == dates[1] & dt$ind_actividad_cliente == 0,]) #455014
#Restrict to customers who are active to begin with
#The user ids we are interested in, these are users who are active in the first month
activeFirstMonthID <- dt[which(dt$fecha_dato == dates[1] & dt$ind_actividad_cliente == 1),]$ncodpers
activeDT <- dt[dt$ncodpers %in% activeFirstMonthID, ]
activeDT <- activeDT[order(ncodpers)]#ORDER BY NCODPERS FOR EASY MERGE

length(unique(activeDT$ncodpers)) #Should be 344154

nrow(activeDT[activeDT$fecha_dato == dates[10] & activeDT$ind_actividad_cliente == 0,]) #17652
#17652 customers became inactive 
activeDT <- data.frame(activeDT)
demographics <- data.frame(demographics)
#Get total products in last known month (10) and first month
#TOTAL PRODUCT FIRST MONTH
TotalProductFirstMonth <- activeDT[activeDT$fecha_dato == dates[1], 14]
for(i in 15:37){
  TotalProductFirstMonth <- TotalProductFirstMonth + activeDT[activeDT$fecha_dato == dates[1], i]
}
#Checking if everything is in the right order
#mean(activeDT[activeDT$fecha_dato == dates[1],]$ncodpers == activeDT[activeDT$fecha_dato == dates[2],]$ncodpers)
#TOTAL PRODUCT LAST MONTH
TotalProductLastMonth <- activeDT[activeDT$fecha_dato == dates[10], 14]
for(i in 15:37){
    TotalProductLastMonth <- TotalProductLastMonth + activeDT[activeDT$fecha_dato == dates[10], i]
}
#Difference
TotalProductDiff <- TotalProductLastMonth - TotalProductFirstMonth
mean(TotalProductDiff)
mean(abs(TotalProductLastMonth - TotalProductFirstMonth)) #Mean absolute difference, just measures change in either direction
var(TotalProductDiff)
var(TotalProductFirstMonth)
var(TotalProductLastMonth)
t.test(TotalProductLastMonth, TotalProductFirstMonth)

cor(TotalProductDiff,activeDT[activeDT$fecha_dato == dates[10],]$ind_actividad_cliente)
productImportance <- 0
#Product importance is (percentage of ppl with that product)^-1
for(i in 14:37){
  
  avg <- mean(activeDT[activeDT$fecha_dato == dates[10],i])
  productImportance[i-13] <- avg^-1
}
productImportance <- log(productImportance)
names(activeDT)[14:37]

?apply
helperFunction <- function(products){
  return(sum(products * productImportance))
}
# test <- activeDT[1,14:37]
# test * productImportance
# helperFunction(test)
# sum(c(1,2,3)*c(2,4,6))
#ProductImportance for each person
productImportanceData <- apply(activeDT[activeDT$fecha_dato == dates[10], 14:37],1,helperFunction)
#Check correlation between product importance and activity
#Ppl with more product importance (product rarity) tend to be active by the last month 
cor(activeDT[activeDT$fecha_dato == dates[10],]$ind_actividad_cliente, productImportanceData)

####
#Put the data together
engineeredFeatures <- data.frame(ncodpers = unique(activeDT$ncodpers), TotProdDiff = TotalProductDiff, TotProdMonth1 = TotalProductFirstMonth, TotProdMonth10 = TotalProductLastMonth, prodImp = productImportanceData)
demographics <- demographics[demographics$ncodpers %in% unique(activeDT$ncodpers), ]
demographics <- demographics[order(ncodpers)]
#Check that ids are in the right order
mean(demographics$ncodpers == unique(activeDT$ncodpers))
mean(demographics$ncodpers == engineeredFeatures$ncodpers)
toReturn <- data.frame(demographics, engineeredFeatures[,2:5])
#Now add some months 
m10 <- dt[dt$fecha_dato == dates[10] & dt$ncodpers %in% engineeredFeatures$ncodpers,][,c(2,14:34,37)]
m10 <- m10[order(ncodpers)]
mean(m10$ncodpers == engineeredFeatures$ncodpers)
colnames(m10)[2:23] <- paste(colnames(m10)[2:23],"M10",sep = "")
toReturn <- merge(toReturn,m10,by="ncodpers")

m5 <- dt[dt$fecha_dato == dates[5] & dt$ncodpers %in% engineeredFeatures$ncodpers,][,c(2,14:34,37)]
m5 <- m5[order(ncodpers)]
mean(m5$ncodpers == engineeredFeatures$ncodpers)
colnames(m5)[2:23] <- paste(colnames(m5)[2:23],"M5",sep = "")
toReturn <- merge(toReturn,m5,by="ncodpers")

#Add response
#activity
responseDT <- dt[dt$fecha_dato == dates[11] & dt$ncodpers %in% engineeredFeatures$ncodpers,][,c(2,12)]
responseDT <- responseDT[order(ncodpers)]
mean(responseDT$ncodpers == engineeredFeatures$ncodpers)
toReturn <- data.frame(finalActivity = responseDT[,2], toReturn)
names(toReturn)[1] <- "finActivity"
344154 - mean(toReturn[,1]) * 344154 #6% cases of interest
toReturn$fecha_dato <- NULL
toReturn$ncodpers <- NULL
toReturn
write.csv(toReturn, file = "milestone3Data.csv", row.names = F)
# #TOTAL NUMBER OF PRODUCTS ADDED AND DROPPED
# #Takes in student ID, only works with activeDT
# helperFunction2 <- function(id){
#   temp <- activeDT[activeDT$ncodpers == id,]
#   #Number of products added
#   nPAM <- rep(0,9)
#   #Number of products dropped
#   nPDM <- rep(0,9)
#   for(j in 1:9){
#     nPAM[j] <- sum(temp[temp$fecha_dato == dates[j],14:37] < temp[temp$fecha_dato == dates[j+1],14:37])
#     nPDM[j] <- sum(temp[temp$fecha_dato == dates[j],14:37] > temp[temp$fecha_dato == dates[j+1],14:37])
#   }
#   #Total Products Added, Total Products Dropped, Var Products Added, Var Products Dropped
#   return(c(sum(nPAM),sum(nPDM),var(nPAM),var(nPDM)))
# }
# #Function takes way to long
# #Look into doing it without the temp dataframe
# #Lessen the amount of dates we look at
# helperFunction3 <- function(id){
#   #Number of products added
#   nPAM <- rep(0,9)
#   #Number of products dropped
#   nPDM <- rep(0,9)
#   for(j in 1:9){
#     nPAM[j] <- sum(activeDT[activeDT$ncodpers == id & activeDT$fecha_dato == dates[j],14:37] < activeDT[activeDT$ncodpers == id & activeDT$fecha_dato == dates[j+1],14:37])
#     nPDM[j] <- sum(activeDT[activeDT$ncodpers == id & activeDT$fecha_dato == dates[j],14:37] > activeDT[activeDT$ncodpers == id & activeDT$fecha_dato == dates[j+1],14:37])
#   }
#   #Total Products Added, Total Products Dropped, Var Products Added, Var Products Dropped
#   return(c(sum(nPAM),sum(nPDM),var(nPAM),var(nPDM)))
# }
# 
# 
# 
# head(unique(activeDT$ncodpers))
# temp <- activeDT[activeDT$ncodpers == 15892,]
# j <- 1
# sum(temp[temp$fecha_dato == dates[j+3],14:37] < temp[temp$fecha_dato == dates[j+3],14:37])
# helperFunction3(15896)
# #IF I RUN THE ALGORITHM THE HEAD OF THE OUT PUT SHOULD BE
# helperFunction3(15889)
# helperFunction3(15890)
# helperFunction3(15892)
# 
# #Tot Add, Tot Drop, Var Add, Var Drop
# length(unique(activeDT$ncodpers))#344154 Customers
# productChangeStatistics <- sapply(unique(activeDT$ncodpers)[809:820],helperFunction3) #Starting at 12:30 end 12:38 for 100 entries 
# #8 min / 100 entry  = 8/100 min for 1 entry , #27532 minutes to do the whole thing, 458 hours
# #CHECK NA
# cor(productChangeStatistics[1,], activeDT$ind_actividad_cliente[809:820])
# #Get stratified sample of indices
# stratifiedSample <- c(1:50,which(activeDT$ind_actividad_cliente == 0)[1:50]) 
# productChangeStatistics <- sapply(unique(activeDT$ncodpers)[stratifiedSample],helperFunction3)
# cor(productChangeStatistics[1,], activeDT[stratifiedSample,]$ind_actividad_cliente)
# cor(productChangeStatistics[2,], activeDT[stratifiedSample,]$ind_actividad_cliente)
# cor(productChangeStatistics[3,], activeDT[stratifiedSample,]$ind_actividad_cliente)
# cor(productChangeStatistics[4,], activeDT[stratifiedSample,]$ind_actividad_cliente)

###Testing 
inactiveFirstMonthID <- dt[which(dt$fecha_dato == dates[1] & dt$ind_actividad_cliente == 0),]$ncodpers
inactiveDT <- dt[dt$ncodpers %in% inactiveFirstMonthID, ]

nrow(inactiveDT[inactiveDT$fecha_dato == dates[10] & inactiveDT$ind_actividad_cliente == 1]) #13440 inactive customers became active, this is a good problem to look at too but it is secondary



