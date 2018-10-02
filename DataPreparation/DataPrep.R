#Justin Tse
#Data Preparation
rm(list=ls())
#Data Cleaning Part 2
#Putting all of the data for 1 customer into 1 line

#Read in the data
#install.packages("data.table")
library(data.table)
dt <- fread("modifiedTrainingData.csv",nrows = -1)
dt$V1 <- NULL
########################################################################################
#Start with Lag 5, keep the Last 6 months of Data
length(unique(dt$fecha_dato))
dates <- unique(dt$fecha_dato)[12:17]
#Restrict data to customers that are present in the first month
dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[1],]$ncodpers) #True if we want to keep these customers
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[1],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[2],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[3],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[4],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[5],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[6],]$ncodpers),]
demographics <- dt[dt$fecha_dato == dates[5],][,c(1:13,35,36,38:51)]
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
#Get Finance Data for Month 1
m1 <- dt[dt$fecha_dato == dates[1],][,c(2,14:34,37)]
m1 <- m1[order(ncodpers)]
colnames(m1)[2:23] <- paste(colnames(m1)[2:23],"M1",sep = "")
#Get Finance Data for Month 2
m2 <- dt[dt$fecha_dato == dates[2],][,c(2,14:34,37)]
m2 <- m2[order(ncodpers)]
colnames(m2)[2:23] <- paste(colnames(m2)[2:23],"M2",sep = "")
#Get Finance Data for Month 3
m3 <- dt[dt$fecha_dato == dates[3],][,c(2,14:34,37)]
m3 <- m3[order(ncodpers)]
colnames(m3)[2:23] <- paste(colnames(m3)[2:23],"M3",sep = "")
#Get Finance Data for Month 4
m4 <- dt[dt$fecha_dato == dates[4],][,c(2,14:34,37)]
m4 <- m4[order(ncodpers)]
colnames(m4)[2:23] <- paste(colnames(m4)[2:23],"M4",sep = "")
#Get Finance Data for Month 5
m5 <- dt[dt$fecha_dato == dates[5],][,c(2,14:34,37)]
m5 <- m5[order(ncodpers)]
colnames(m5)[2:23] <- paste(colnames(m5)[2:23],"M5",sep = "")

lag5dt <- merge(demographics,m1,by="ncodpers")
lag5dt <- merge(lag5dt,m2,by="ncodpers")
lag5dt <- merge(lag5dt,m3,by="ncodpers")
lag5dt <- merge(lag5dt,m4,by="ncodpers")
lag5dt <- merge(lag5dt,m5,by="ncodpers")
#Get total number of products
m5 <- data.frame(m5)
totalProduct <- m5[,2]+m5[,3]
for(i in 4:23){
  totalProduct<- totalProduct + m5[,i]
}
totalProduct
#Check if Im gonna merge them correctly
head(lag5dt$ncodpers,10)
head(m5$ncodpers,10)
lag5dt <- cbind(totalProduct, lag5dt)
#Get Finance Data for Month 6
m6 <- dt[dt$fecha_dato == dates[6],][,c(2,14:34,37)]
m6 <- m6[order(ncodpers)]
colnames(m6)[2:23] <- paste(colnames(m6)[2:23],"RESPONSE",sep = "")

lag5dt <- merge(lag5dt,m6,by="ncodpers")

#Get rid of ncodpers and fecha_dato
lag5dt$ncodpers<-NULL
lag5dt$fecha_dato<-NULL

#Write the CSV
?write.csv
write.csv(lag5dt, file = "lag5Data.csv", row.names = F)
tenPer <- as.integer(nrow(lag5dt)*.1)
set.seed(1)
lag5randomSample <-  lag5dt[sample(nrow(lag5dt), tenPer), ]
write.csv(lag5randomSample, file = "lag5Data10percentSample.csv", row.names = F)
#Get a 10 percent sample
########################################################################################
#Do Lag 2, keep the last 2 months of Data
length(unique(dt$fecha_dato))
dates <- unique(dt$fecha_dato)[15:17]
#Restrict data to customers that are present in the first month
dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[1],]$ncodpers) #True if we want to keep these customers
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[1],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[2],]$ncodpers),]
dt <- dt[dt$ncodpers %in% unique(dt[dt$fecha_dato == dates[3],]$ncodpers),]
demographics <- dt[dt$fecha_dato == dates[3],][,c(1:13,35,36,38:51)]
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
#Get Finance Data for Month 1
m1 <- dt[dt$fecha_dato == dates[1],][,c(2,14:34,37)]
m1 <- m1[order(ncodpers)]
colnames(m1)[2:23] <- paste(colnames(m1)[2:23],"M1",sep = "")
#Get Finance Data for Month 2
m2 <- dt[dt$fecha_dato == dates[2],][,c(2,14:34,37)]
m2 <- m2[order(ncodpers)]
colnames(m2)[2:23] <- paste(colnames(m2)[2:23],"M2",sep = "")

lag2dt <- merge(demographics,m1,by="ncodpers")
lag2dt <- merge(lag2dt,m2,by="ncodpers")

#Get total number of products
m2 <- data.frame(m2)
totalProduct <- m2[,2]+m2[,3]
for(i in 4:23){
  totalProduct<- totalProduct + m2[,i]
}
totalProduct
#Check if Im gonna merge them correctly
head(lag2dt$ncodpers,10)
head(m2$ncodpers,10)
lag2dt <- cbind(totalProduct, lag2dt)
#Get Finance Data for Month 6
m3 <- dt[dt$fecha_dato == dates[3],][,c(2,14:34,37)]
m3 <- m3[order(ncodpers)]
colnames(m3)[2:23] <- paste(colnames(m3)[2:23],"RESPONSE",sep = "")

lag2dt <- merge(lag2dt,m3,by="ncodpers")

#Get rid of ncodpers and fecha_dato
lag2dt$ncodpers<-NULL
lag2dt$fecha_dato<-NULL

#Write the CSV
?write.csv
write.csv(lag2dt, file = "lag2Data.csv", row.names = F)
tenPer <- as.integer(nrow(lag2dt)*.1)
set.seed(1)
lag2randomSample <-  lag2dt[sample(nrow(lag2dt), tenPer), ]
write.csv(lag2randomSample, file = "lag2Data10percentSample.csv", row.names = F)

#Then do Lag, 15, keep the last 15 months of Data