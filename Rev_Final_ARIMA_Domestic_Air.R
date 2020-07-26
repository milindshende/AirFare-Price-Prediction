
################# Libraries ###################
library(tseries)
library(forecast)
library(quantmod)
library(timeSeries)
library(Metrics)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
#################### EDA Data Preparation ##############
airfare_rev <- fread(file.choose()) # Import the original Client data
airfare_rev$InvoiceDate <- dmy_hm(airfare_rev$InvoiceDate) 
airfare_rev <- airfare_rev %>%
  mutate(Year = year(airfare_rev$InvoiceDate),Month =month(airfare_rev$InvoiceDate,label = TRUE),
         Day = wday(airfare_rev$InvoiceDate,label = TRUE),Date = day(airfare_rev$InvoiceDate),Hour = hour(airfare_rev$InvoiceDate),
         Minutes = minute(airfare_rev$InvoiceDate))
airfare_rev$NetFare <- as.numeric(airfare_rev$NetFare) # change to numeric
airfare_rev<- airfare_rev[,2:10] # removed InvoiceDate as we have splitted & added
########## Missing Value #########
sum(is.na(airfare_rev)) # 60903
sapply(airfare_rev,function(x) sum(is.na(x))) # 60891,0,0,2,2,2,2,2,2
# Replace blank fields with NA in PT,IT
airfare_rev$ProductType [airfare_rev$ProductType==""] <- NA
airfare_rev$ItineraryType [airfare_rev$ItineraryType==""] <-NA
sum(is.na(airfare_rev)) # 93682
sapply(airfare_rev,function(x) sum(is.na(x))) # 60891,2,32777,2,2,2,2,2,2
####### Missing Value Imputation ######
# Define Mode Function 
getmode <- function(v) {
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v , uniqv)))] }
# Imputation of missing values with mode in ItineryType
getmode(airfare_rev$ItineraryType) # Domestic
airfare_rev$ItineraryType[is.na(airfare_rev$ItineraryType)] <- getmode(airfare_rev$ItineraryType)
# Removing NA from ProductType
airfare_rev<- airfare_rev%>%
  drop_na(ProductType)
##### Imputate missing values of NetFare based on ProductType
# First crosscheck NA values in netfare based on PT 'refund' & 'payment' prior to imputation
airfare_rev %>%
  filter(ProductType=="refund")%>%
  group_by(NetFare)%>%
  summarise(number=n(),mean_Netfare = mean(NetFare),Minimum=min(NetFare),Maximum=max(NetFare))
# Found 5495 records with NA value in NetFare under PT 'refund'
airfare_rev %>%
  filter(ProductType=="payment")%>%
  group_by(NetFare)%>%
  summarise(number=n(),mean_Netfare = mean(NetFare),Minimum=min(NetFare),Maximum=max(NetFare))
# Found 55394 records with NA value in NetFare under PT 'payment'
airfare_rev$NetFare[is.na(airfare_rev$NetFare)] <- 0
sapply(airfare_rev,function(x) sum(is.na(x))) # 0,0,0,0,0,0,0,0,0 
##### Split the data based on Itinerytype for future model building ####
# Create new dataset for domestic 
domestic <- airfare_rev %>%
  filter(ItineraryType=="Domestic")
# Create new dataset for international
international <- airfare_rev %>%
  filter(ItineraryType=="International")
###################### ARIMA ############
# Prepare new Variable InvDate by merging (Year,Month,Date)
InvDate <- with(domestic,dmy(sprintf('%02d%02d%04d',Date,Month,Year))) 
domestic_11 <- cbind(domestic,InvDate)
domestic_12 <- domestic_11 %>%
  filter(ProductType=="Air")%>%
  group_by(InvDate) %>%
  summarise(MNetFare=mean(NetFare,na.rm = T))
######################
str(domestic_12$InvDate) # Date
dom_ts <- xts(domestic_12$MNetFare,order.by = domestic_12$InvDate,frequency = 365)
autoplot(dom_ts)
Pacf(dom_ts,lag.max = 20)
pacf(dom_ts)
Acf(dom_ts,lag.max = 20)
adf.test(dom_ts) # p-value=0.0186 Stationary , d=0
dom_ts_d1 <- diff(dom_ts,differences=1)
adf.test(dom_ts_d1) # p-value= 0.01 Stationary , d=1 *
dom_ts_d2 <- diff(dom_ts,differences=2)
adf.test(dom_ts_d) # p-value= 0.01 Stationary , d=2
autoplot(dom_ts_d1)
Pacf(dom_ts_d1,lag.max = 20) # p=1 *
Acf(dom_ts_d1,lag.max = 20) # q=7 *
dom_arima <- Arima(y=dom_ts,order = c(1,1,1))
forecast(dom_arima,h=30)
autoplot(forecast(dom_arima,h=30)) # x axis = Date in numbers

# To get X axis as Dates ->
require(lubridate)
dom_ts <- ts(domestic_12$MNetFare,start = c(2018,yday("2018-04-01")),frequency = 365)
plot(forecast(ets(dom_ts),30),xaxt="n")
a=seq(as.Date("2018-04-01"),by="days",length=475)
axis(1,at=decimal_date(a),labels = format(a,"%Y %b %d"),cex.axis=0.6)
abline(v=decimal_date(a),color="grey",lwd=0.1)


####################### 
tsData_air <- ts(domestic_12$MNetFare, start = min(domestic_12$InvDate),end = max(domestic_12$InvDate), frequency = 1)
summary(tsData_air)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2016  4351    4775    4821    5234    8770
################################################
# ADF Test for p-value
print(adf.test(tsData_air))
# Dickey-Fuller = -3.84, Lag order = 7, p-value = 0.017
# alternative hypothesis: stationary
############## ARIMA Model without any transformation #####
(arima_model_22 <- arima(tsData_air,c(0,0,2),seasonal = list(order=c(0,0,2),period=90)))
arima_pred_1 <- predict(arima_model_22, n.ahead = 90)
ts.plot(tsData_air,arima_pred_1$pred, log="y",lty=c(1,3))
plot(forecast(arima_model_22,h=90))
pred_date<- as.Date(as.numeric(time(arima_pred_1$pred)))
pred_domestic_airfare<- data.frame(pred_date,arima_pred_1$pred)
colnames(pred_domestic_airfare) <- c("Date","MeanNetFare")
View(pred_domestic_airfare)
###################### END ###############