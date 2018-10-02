#Justin Tse
#Basic Visualization
rm(list=ls())
library(ggplot2)
library(data.table)
dt <- fread("train_ver2.csv",nrows = -1)
lastmonthdata <- dt[dt$fecha_dato == "2016-05-28",]
lastmonthdata <- data.frame(lastmonthdata)
firstmonthdata <- dt[dt$fecha_dato == "2015-01-28",]
firstmonthdata <- data.frame(firstmonthdata)
#Histogram of Age
qplot(lastmonthdata$age, geom="histogram",xlab = "Age", main = "Histogram of Age", ylab = "Count", xlim = c(0,100), bins = 50) 
ggplot(lastmonthdata, aes(x=age)) + 
  geom_density(color="black", fill="lightblue")+ 
  xlab("Age") + ylab("Density") + xlim(0,120)+
  ggtitle("Density Plot of Age", subtitle = NULL)

#Histogram of Rent
renta <- data.frame("a" = lastmonthdata[!is.na(lastmonthdata$renta),]$renta)

qplot(renta$a, geom="histogram",xlab = "Gross Household Income", main = "Histogram of Gross Household Income", ylab = "Count", xlim= c(0,600000)) 
ggplot(renta, aes(x=a)) + 
  geom_density(color="black", fill="lightblue")+ 
  xlab("Gross Household Income") + ylab("Density") + xlim(0,600000)+
  ggtitle("Household Gross Income", subtitle = NULL)
max(lastmonthdata$renta,na.rm=T)

#Names of the Products
products <- c("Saving Account", "Guarantees", "Current Accounts", "Derivada Account", 
  "Payroll Account", "Junior Account", "Mas particular Account", "particular Account", "particular Plus Account", "Short-term deposits",
  "Medium-term deposits", "Long-term deposits", "e-account", "Funds", "Mortgage",
  "Pensions", "Loans", "Taxes", "Credit Card", "Securities", "Home Account", "Payroll", "Pensions", "Direct Debit")
names(lastmonthdata)[25:48] <- products
names(lastmonthdata)

#Get percentage of ppl with each product in Last Month who were customers in the first month
percentageProductLastMonth <- c(1:24)
customerIDfirstmonth <- firstmonthdata$ncodpers
lastmonthlongcustomers <- lastmonthdata[lastmonthdata$ncodpers %in% customerIDfirstmonth,]
customerIDfirstmonth <- lastmonthlongcustomers$ncodpers
firstmonthlongcustomers <- firstmonthdata[firstmonthdata$ncodpers %in% customerIDfirstmonth,]
for(i in 25:48){
  percent <- sum(lastmonthlongcustomers[,i])/nrow(lastmonthlongcustomers )
  print(products[i-24])
  print(percent)
  percentageProductLastMonth[i-24] <- percent
}
#Get percentage of ppl with each product in the first month
percentageProductFirstMonth <- c(1:24)
for(i in 25:48){
  percent <- sum(firstmonthlongcustomers[,i], na.rm = T)/nrow(firstmonthlongcustomers)
  print(products[i-24])
  print(percent)
  percentageProductFirstMonth[i-24] <- percent
}
#Customer percents contains percentage data on the long time customers

percentageProductLastMonth- percentageProductFirstMonth
names(percentageProductFirstMonth) <- products
plot(percentageProductLastMonth)
customerPercents <- data.frame("firstmonth" = percentageProductFirstMonth * 100, "lastmonth" = percentageProductLastMonth* 100, "difference"=percentageProductLastMonth*100-percentageProductFirstMonth* 100)
#customerPercents <- t(customerPercents)
customerPercents <- data.frame(customerPercents, "names"= products)

#colnames(customerPercents) <- c("first", "last", "last - first")
rownames(customerPercents) <- make.names(products, unique = T)

ggplot(customerPercents, aes(names, firstmonth))+ geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ 
  xlab("Products") + ylab("Percentage")+
  ggtitle("Percentage of Product for Customers in the First Month", subtitle = NULL)

ggplot(customerPercents, aes(names, lastmonth))+ geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ 
  xlab("Products") + ylab("Percentage")+
  ggtitle("Percentage of Product for Customers in the Last Month", subtitle = NULL)

customerPercents$difference <- NULL

tester <- melt(customerPercents, id.vars='names')
ggplot(tester, aes(names, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ 
  xlab("Products") + ylab("Percentage")+
  ggtitle("Percentage of Product for Customers", subtitle = NULL)+ 
  scale_fill_manual(labels = c("First Month", "Last Month"),values=c('#3E606F','#193441'))

#Total Products For Last Month
totalProduct <- lastmonthdata[,25]+lastmonthdata[,26]
for(i in 27:48){
  totalProduct<- totalProduct + lastmonthdata[,i]
}
#Change columns to numerics
lastmonthdata$sexo <- lastmonthdata$sexo == "V"
lastmonthdata$indresi <- lastmonthdata$indresi == "S"
lastmonthdata$indext <- lastmonthdata$indext == "S"
#Get Correlations
a<-cor(lastmonthdata$sexo, totalProduct)
b<-cor(lastmonthdata$age, totalProduct)
c<-cor(lastmonthdata$antiguedad, totalProduct)
d<-cor(lastmonthdata$ind_nuevo, totalProduct)
e<-cor(lastmonthdata$indext, totalProduct)
f<-cor(lastmonthdata$ind_actividad_cliente, totalProduct)
g<-cor(lastmonthdata$renta, totalProduct, use= "complete.obs")
h<-cor(lastmonthdata$pais_residencia == "ES", totalProduct)
i<-cor(lastmonthdata[,28], totalProduct)
correlation <- c(a,b,c,d,e,f,g,h,i)
corNames <- c("Sex", "Age", "Customer Seniority", "New Customer","Foreigner", "Active", "Gross Income", "In Spain","Current Accounts")
correlation <- data.frame(correlation, corNames)
plot(correlation)
ggplot(correlation, aes(corNames, correlation)) +   
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ 
  xlab("Features") + ylab("Correlation")+
  ggtitle("Correlation Between Features and Total Products", subtitle = NULL)
#Get percent of customers in firstmonth who become inactive by the last month

#Set NA value to 1
for(i in nrow(firstmonthlongcustomers)){
  if(is.na(firstmonthlongcustomers$ind_actividad_cliente[i])){
    firstmonthlongcustomers$ind_actividad_cliente[i] <-0
  }
}
#Number inactive in first month
sum(firstmonthlongcustomers$ind_actividad_cliente, na.rm=T)+
  sum(is.na(firstmonthlongcustomers$ind_actividad_cliente))
#Number inactive in last month
sum(lastmonthlongcustomers$ind_actividad_cliente)
#611882 Total customers
329075 - 322444 #Num customers who became inactive
#Compare those customers who BECAME inactive with the inactive, and active customers.

#Compare Segments
#Split last month data into segments
unique(dt$segmento)
lastmonthdata <- cbind(totalProduct,lastmonthdata)
seg1 <- lastmonthdata[lastmonthdata$segmento == unique(dt$segmento)[4],]
seg2 <- lastmonthdata[lastmonthdata$segmento == unique(dt$segmento)[1],]
seg3 <- lastmonthdata[lastmonthdata$segmento == unique(dt$segmento)[2],]
#Age
mean(seg1$age)
mean(seg2$age)
mean(seg3$age)
segAge <- data.frame(age = c(mean(seg1$age),mean(seg2$age),mean(seg3$age)), "names" = c("Top","Particulars","Students"))
ggplot(segAge, aes(names, age))+ geom_bar(stat = "identity", fill = '#7B1F25')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ 
  xlab("Segments") + ylab("Average Age")+
  ggtitle("Average Age vs. Segment", subtitle = NULL)
#Renta
mean(seg1$renta, na.rm=T)
mean(seg2$renta, na.rm=T)
mean(seg3$renta, na.rm=T)
segRenta <- data.frame(renta = c(mean(seg1$renta, na.rm=T),mean(seg2$renta, na.rm=T),mean(seg3$renta, na.rm=T)), "names" = c("Top","Particulars","Students"))
ggplot(segRenta, aes(names, renta))+ geom_bar(stat = "identity", fill = '#7AA69C')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ 
  xlab("Segments") + ylab("Average Gross Income")+
  ggtitle("Average Gross Income vs. Segment", subtitle = NULL)
#Total Product
mean(seg1$totalProduct, na.rm=T)
mean(seg2$totalProduct, na.rm=T)
mean(seg3$totalProduct, na.rm=T)
segTotal <- data.frame(total= c(mean(seg1$totalProduct, na.rm=T),mean(seg2$totalProduct, na.rm=T),mean(seg3$totalProduct, na.rm=T)), "names" = c("Top","Particulars","Students"))
ggplot(segTotal, aes(names, total))+ geom_bar(stat = "identity", fill = '#DCA046')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ 
  xlab("Segments") + ylab("Average Number of Products")+
  ggtitle("Average Number of Products vs. Segment", subtitle = NULL)
#Seniority
mean(seg1$antiguedad)
mean(seg2$antiguedad)
mean(seg3$antiguedad)
segSen <- data.frame(total= c(mean(seg1$antiguedad, na.rm=T),mean(seg2$antiguedad, na.rm=T),mean(seg3$antiguedad, na.rm=T)), "names" = c("Top","Particulars","Students"))
ggplot(segSen, aes(names, total))+ geom_bar(stat = "identity", fill = '#BF6262')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ 
  xlab("Segments") + ylab("Average Seniority in Months")+
  ggtitle("Average Seniority vs. Segment", subtitle = NULL)
