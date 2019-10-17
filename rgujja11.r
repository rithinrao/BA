## 1a ##
pnorm(700,mean=494,sd=100,lower.tail = FALSE)

## 1b ##
pnorm(450,mean=494,sd=100)-pnorm(350,mean=494,sd=100)

## 2 ##
z<-qnorm(0.8665)
mean<-449-(z*36)
mean

## 3 ##

kent<-c(59,68,78,60)
la<-c(90,82,78,75)
kentm<-mean(kent)
lam<-mean(la)
yy1<-sum((kent-kentm)*(la-lam))
yy2<-sqrt(sum((kent-kentm)*(kent-kentm)))*sqrt(sum((la-lam)*(la-lam)))
yy1/yy2

## 4 ##

ba1<-read.csv("Online_Retail.csv")
library(dplyr)
f1<-summarise(group_by(ba1,Country),Count=n())
f2<-as.data.frame(f1)
f3<-select(f2,Count)
percent<-(f3/sum(f3))*100
f4<-cbind(f2,percent)
names(f4)[3]<-"percentage"
filter(f4,percent>=1)

## 5 ##

Transactionvalue<-ba1$Quantity*ba1$UnitPrice
ba2<-cbind(ba1,Transactionvalue)
head(ba2)

## 6 ##

f5<-(summarise(group_by(ba2,Country),totalsum=sum(Transactionvalue))%>%filter(totalsum>130000))
f6<-as.data.frame(f5)
f6$Country

## 8 ##

library(ISLR)
f7<-select(ba2,8,9)%>%filter(Country=="Germany")
hist(log(f7$Transactionvalue),main = "Transaction of germany", xlab="Transaction values",col = "blue")

## 9 ##

c9 <- tapply(ba2$Transactionvalue, ba2$CustomerID, length)
c9[which.max(c9)]

d9 <- tapply(ba2$Transactionvalue, ba2$CustomerID, sum)
d9[which.max(d9)]

## 10 ##

colMeans(is.na(ba1))*100

## 11 ##

fun1<-function(x){
  z<-sum(is.na(x))
  return(z)}
tapply(ba2$CustomerID,ba2$Country,fun1)
             ## or ##
b11<-summarise(group_by(ba2,Country),Navalues=fun1(CustomerID))
as.data.frame(b11)

## 13 ##

b13<-ba2%>%filter(Country=="France")
b14<-filter(b13,Quantity<0)
nrow(b14)/nrow(b13)*100 ## with respect to french customers ##
nrow(b14)/nrow(ba2)*100 ## with respect to total customers ##

## 14 ##

b14<-summarise(group_by(ba2,Description),highvalue=sum(Transactionvalue))
c14<-as.data.frame(b14)          
c14[which.max(c14$highvalue),]

## 15 ##

length(unique(ba2$CustomerID))

## 7 ##
Temp=strptime(ba1$InvoiceDate,format='%m/%d/%Y %H:%M',tz='GMT')
ba1$New_Invoice_Date <- as.Date(Temp)
ba1$New_Invoice_Date[20000]- ba1$New_Invoice_Date[10]
ba1$Invoice_Day_Week= weekdays(ba1$New_Invoice_Date)
ba1$New_Invoice_Hour = as.numeric(format(Temp, "%H"))
ba1$New_Invoice_Month = as.numeric(format(Temp, "%m"))

## 7a ##

c7<-summarise(group_by(ba1,ba1$Invoice_Day_Week),count=n())
c8<-as.data.frame(c7)
c8$count/sum(c8$count)*100
               ## or ##
 tapply(ba1$Quantity,ba1$Invoice_Day_Week,NROW) / NROW(ba1$Quantity) * 100


## 7b ##

d7<-summarise(group_by(ba1,ba1$Invoice_Day_Week),sum=sum(Quantity))
d8<-as.data.frame(d7)
d8$sum/sum(d8$sum)*100
              ## or ##
 tapply(ba1$Quantity,ba1$Invoice_Day_Week,sum) / sum(ba1$Quantity) * 100


## 7c ##

e7<-summarise(group_by(ba1,ba1$New_Invoice_Month),sum=sum(Quantity))
e8<-as.data.frame(e7)
e8$sum/sum(e8$sum)*100
               ## or ##
 tapply(ba1$Quantity,ba1$New_Invoice_Month,sum) / sum(ba1$Quantity) * 100

## 7d ##

f7<-ba1%>%filter(Country=="Australia")
f8<-summarise(group_by(f7,Country),high=max(Quantity))
f9<-as.data.frame(f8)
f10<-filter(f7,Quantity==1152)
select(f10,Invoice_Day_Week)

## 7e ##

## install.packages("zoo") ##
library(zoo)
r <- table(ba1$New_Invoice_Hour)
r
rollapply(r, 2, sum)

## 12 ##

c12<-select(ba2,c(5,7))
head(c12)
levels(c12$InvoiceDate)
diff(c12$InvoiceDate,lag=1)
