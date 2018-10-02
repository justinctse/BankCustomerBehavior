#Justin Tse
#DataCleaning
#Read in the data
#install.packages("data.table")
library(data.table)
dt <- fread("train_ver2.csv",nrows = -1)
head(dt,5)

userA <- dt[dt$ncodpers == 1050611,]
userA
unique(dt$ind_tjcr_fin_ult1)
dt2 <- dt[1:6000,]
?sample
samp <- sample.int(6000000,size = 100)
dt2 <- dt[samp,]

unique(dt$fecha_dato)

#number of unique useres, 956645
length(unique(dt$ncodpers))


dt <- data.frame(dt)
lastmonthdata <- dt[dt$fecha_dato == "2016-05-28",]
secondlastmonthdata <- dt[dt$fecha_dato == "2016-04-28",]
firstmonthdata <- dt[dt$fecha_dato == "2015-01-28",]
#I and P are dissatisfied
sum(lastmonthdata$tiprel_1mes == "I") #534429 inactive customers 
534429/931453 #0.5737584
sum(lastmonthdata$tiprel_1mes == "P") #330 customers who have left
330/931453 #0.0003542852

sum(firstmonthdata$tiprel_1mes == "I") #294902 inactive customers 
294902/625457 #0.4714984
sum(firstmonthdata$tiprel_1mes == "P") #29 customers who have left
29/625457

unique(dt$ind_ahor_fin_ult1)
unique(dt$ind_tjcr_fin_ult1)

sum(lastmonthdata[,25:48])
?sapply

sapply(lastmonthdata[,25:48],sum)/931453 #Proportions for last month data
sapply(firstmonthdata[,25:48],sum)/625457 


############################################################
#Check NA values in each column
for(i in 1:ncol(dt)){
  numNA <- sum(is.na(dt[,i]))
  print(paste("Column",i,"has",numNA,"NA values."))
}
#renta has lots of NA values 

#Look at each variable
#Check how many first month customers are NOT in last month customers
firstMonthCustomers <- firstmonthdata$ncodpers
lastMonthCustomers <- lastmonthdata$ncodpers
sum((firstMonthCustomers %in% lastMonthCustomers) & 0) #customers in first month and NOT in last month

unique(dt$ind_empleado)
sum(dt$ind_empleado == "") #27734 empty values

unique(dt$pais_residencia) #A lot of vars, leave out for now, or simplify to two countries
#13553710 are in ES
#93599 in other countries
for(s in unique(dt$pais_residencia)){
  print(s)
  print(sum(dt$pais_residencia == s)) 
}
#V is male #H is female
unique(dt$sexo)
sum(dt$sexo == "")
sum(lastmonthdata$sexo == "")
sum(firstmonthdata$sexo == "")

unique(dt$age)
hist(dt$age)

#when the customer joined the bank
sum(dt$fecha_alta == "")
sum(lastmonthdata$fecha_alta == "") #last month data isnt missing any

unique(dt$ind_nuevo)
sum(is.na(dt$ind_nuevo))
sum(is.na(lastmonthdata$ind_nuevo))
sum(is.na(secondlastmonthdata$ind_nuevo))

#one value is -999999
unique(dt$antiguedad)
sum(dt$antiguedad == 157)
sum(is.na(dt$antiguedad))
sum(is.na(lastmonthdata$antiguedad))
sum(is.na(secondlastmonthdata$antiguedad))

sum(secondlastmonthdata$antiguedad == -999999)
dt$antiguedad[262]

unique(dt$indrel)
sum(is.na(dt$indrel))
sum(is.na(secondlastmonthdata$indrel))

unique(dt$ult_fec_cli_1t)

unique(dt$indrel_1mes)
sum(dt$indrel_1mes=="")
sum(secondlastmonthdata$indrel_1mes == "")

unique(dt$tiprel_1mes)
sum(dt$tiprel_1mes == "")
sum(secondlastmonthdata$tiprel_1mes == "")

unique(dt$indresi)
sum(dt$indresi == "")
sum(secondlastmonthdata$indresi == "")

unique(dt$indext)
sum(secondlastmonthdata$indext == "")

unique(dt$conyuemp)
sum(secondlastmonthdata$conyuemp == "")

unique(dt$canal_entrada)

unique(dt$indfall)
sum(secondlastmonthdata$indfall == "S")
sum(secondlastmonthdata$indfall == "")

unique(dt$tipodom)
sum(is.na(dt$tipodom))

unique(dt$cod_prov)

unique(dt$nomprov)

unique(dt$ind_actividad_cliente)
sum(is.na(dt$ind_actividad_cliente))

unique(dt$renta)
mean(dt$renta, na.rm = T)

unique(dt$segmento)
sum(secondlastmonthdata$segmento == "")

#Watch for 46 and 47
#All columns are good EXCEPT for 46 and 47
for(i in 25:48){
  print(unique(dt[,i]))
}
#Pension and payroll
sum(is.na(dt[,46]))
sum(is.na(dt[,47]))
sum(is.na(secondlastmonthdata[,46]))
sum(is.na(secondlastmonthdata[,47]))
#Use last month data on these 

#################################################################

#Get list of names of people who are in Last month but not in second to last month
!(lastmonthdata$ncodpers %in% secondlastmonthdata$ncodpers) #True if customer is in last month and not in second to last month
customersToDelete <- lastmonthdata$ncodpers[!(lastmonthdata$ncodpers %in% secondlastmonthdata$ncodpers)] #roughly 5000 customers

#Delete from dt
sum(dt$ncodpers %in% customersToDelete) #How many we are deleting
dt$ncodpers %in% customersToDelete #True if we want to delete it, so keep the complement of this
!(dt$ncodpers %in% customersToDelete)
dt <- dt[!(dt$ncodpers %in% customersToDelete),]
#13647309 rows to start, 13642014 to end
#Update my other data frames
lastmonthdata <- dt[dt$fecha_dato == "2016-05-28",]
secondlastmonthdata <- dt[dt$fecha_dato == "2016-04-28",]
firstmonthdata <- dt[dt$fecha_dato == "2015-01-28",]
#Delete rows where segmento == ""
sum(dt$segmento == "")
sum(secondlastmonthdata$segmento == "")
sum(lastmonthdata$segmento == "")
#get Id of people who dont have segmento
secondlastmonthdata$ncodpers[secondlastmonthdata$segmento == ""] 
!(dt$ncodpers %in% secondlastmonthdata$ncodpers[secondlastmonthdata$segmento == ""]) 
dt <- dt[!(dt$ncodpers %in% secondlastmonthdata$ncodpers[secondlastmonthdata$segmento == ""]),]
#13607345 to end, 34k deleted

#Modify ind_empleado
unique(dt$ind_empleado)
dt$ind_empleado[dt$ind_empleado == ""] <- "N"
unique(dt$ind_empleado)

#Modify pais_residencia
inSpain <- dt$pais_residencia == "ES"
  #Delete old column
dt$pais_residencia <- NULL
dt <- data.frame(dt,inSpain)
#Update my other data frames
lastmonthdata <- dt[dt$fecha_dato == "2016-05-28",]
secondlastmonthdata <- dt[dt$fecha_dato == "2016-04-28",]
firstmonthdata <- dt[dt$fecha_dato == "2015-01-28",]

#Modify sexo
sum(dt$sexo == "")
sum(secondlastmonthdata$sexo =="")
secondlastmonthdata$ncodper[secondlastmonthdata$sexo == ""] #These are the ids that we want to delete

dt <- dt[!(dt$ncodpers == secondlastmonthdata$ncodper[secondlastmonthdata$sexo == ""]),]

#modify fecha alta
dt$fecha_alta <- NULL

#modify antiguedad, delete customers with seniority -999999
secondlastmonthdata$ncodpers[secondlastmonthdata$antiguedad == -999999]
dt$ncodpers %in% secondlastmonthdata$ncodpers[secondlastmonthdata$antiguedad == -999999] #These are the columns to delete
dt <- dt[!(dt$ncodpers %in% secondlastmonthdata$ncodpers[secondlastmonthdata$antiguedad == -999999]),]
#update my helper data frames
lastmonthdata <- dt[dt$fecha_dato == "2016-05-28",]
secondlastmonthdata <- dt[dt$fecha_dato == "2016-04-28",]
firstmonthdata <- dt[dt$fecha_dato == "2015-01-28",]

#modify ult_fec_cli_1t
ultCopy <- dt$ult_fec_cli_1t
unique(ultCopy)
sum(ultCopy == "")
#Way too many missing values just delete
ultCopy <- NULL
dt$ult_fec_cli_1t <- NULL

#modify indrel1_mes
#Customer type at beginning of month
unique(dt$indrel_1mes)[2]
sum(dt$indrel1_mes == unique(dt$indrel_1mes)[2])

cTypeBeg1 <- dt$indrel_1mes %in% c("1", "1.0")
cTypeBeg2 <- dt$indrel_1mes %in% c("2", "2.0")
cTypeBeg3 <- dt$indrel_1mes %in% c("3", "3.0")
cTypeBeg4 <- dt$indrel_1mes %in% c("4", "4.0")
cTypeBegP <- dt$indrel_1mes == "P"
dt$indrel_1mes <- NULL
dt <- data.frame(dt, cTypeBeg1, cTypeBeg2, cTypeBeg3, cTypeBeg4, cTypeBegP)

#Modify tiprel_1mes
unique(dt$tiprel_1mes)
sum(dt$tiprel_1mes== "")
#update my helper data frames
secondlastmonthdata <- dt[dt$fecha_dato == "2016-04-28",]
sum(secondlastmonthdata$tiprel_1mes == "")

dt$ncodpers[dt$tiprel_1mes == ""]
dt <- dt[!(dt$tiprel_1mes == ""),]
  #Turn it to categorical
cusRelA <- dt$tiprel_1mes == "A" 
cusRelI <- dt$tiprel_1mes == "I" 
cusRelP <- dt$tiprel_1mes == "P" 
cusRelR <- dt$tiprel_1mes == "R" 
cusRelN <- dt$tiprel_1mes == "N" 
dt$tiprel_1mes <- NULL
dt<-data.frame(dt,cusRelA, cusRelI, cusRelN, cusRelP, cusRelR)

#Modify conyuemp
unique(dt$conyuemp)
sum(dt$conyuemp == "")
sum(dt$conyuemp == "N")
sum(dt$conyuemp == "S")
dt$conyuemp <- NULL
dt$canal_entrada <- NULL

#Check who is dead (Modify indfall)
dt$ncodpers[dt$indfall == "S"]
dt <- dt[!(dt$indfall=="S"),]

#modify tipdom
dt$tipodom <- NULL


# #modify cod_prov
# unique(dt$cod_prov)[52] #ENTRY 52 is NA
# sort(unique(dt$cod_prov))
# class(dt$cod_prov)
# temp <- data.frame(dt$cod_prov == 1)
# sum(dt$cod_prov == 1, na.rm = T)
# for(i in 1:52){
#   name <- paste("cod_prov",unique(dt$cod_prov)[i],sep = "")
#   tempVec <- dt$cod_prov == unique(dt$cod_prov)[i]
#   dt <- data.frame(dt, name = tempVec)
# }

#DO CASE 53 manually


# cod_prov_cat<-model.matrix( ~ factor(cod_prov) - 1, data = dt)
# dt$cod_prov <- NULL
# cod_prov_cat <- data.frame(cod_prov_cat)
# dt <- data.frame(dt, cod_prov_cat[,1:10])

#Modify ind_actividad_cliente
#Set NA values to 0
unique(dt$ind_actividad_cliente)

#Modify renta
unique(dt$renta)
sum(is.na(dt$renta))
#Replace na values with mean
# mean <- mean(dt$renta, na.rm = T)
# for(i in 1:nrow(dt)){
#   if(is.na(dt$renta[i])){
#     dt$renta[i] <- mean
#   }
# }
dt$renta[is.na(dt$renta)]<-mean
# x <- c(1,1,1,1,1)
# bool <- c(T,T,F,F,T)
# x[bool] <- 5
# x
#modify segmento
unique(dt$segmento)
sum(dt$segmento == "")
segmento1 <- dt$segmento == "01 - TOP" 
segmento2 <- dt$segmento == "02 - PARTICULARES"
segmento3 <- dt$segmento == "03 - UNIVERSITARIO"
dt$segmento <- NULL
dt <- data.frame(dt,segmento1, segmento2, segmento3)

dt$nomprov <- NULL
dt$cod_prov <- NULL#Setting to NULL for now
#
write.csv(dt, file = "modifiedTrainingData.csv")
