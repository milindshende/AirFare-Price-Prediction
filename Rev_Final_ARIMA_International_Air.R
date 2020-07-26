
################# Libraries ###################
library(tseries)
library(forecast)
library(quantmod)
library(timeSeries)
library(Metrics)
library(lubridate)
library(dplyr)
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
################### ARIMA ############
# Prepare new Variable InvDate by merging (Year,Month,Date)
InvDate_2 <- with(international,dmy(sprintf('%02d%02d%04d',Date,Month,Year))) 
international_11 <- cbind(international,InvDate_2)
international_12 <- international_11 %>%
  filter(ProductType=="Air")%>%
  group_by(InvDate_2) %>%
  summarise(MNetFare=mean(NetFare,na.rm = T))
#####################
ts_international_air <- ts(international_12$MNetFare, start = min(international_12$InvDate_2),max(international_12$InvDate_2), frequency = 1)
summary(ts_international_air)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-26871   16739   22674   27926   33579  436366
################################################
# ADF Test for p-value
print(adf.test(ts_international_air))
# Dickey-Fuller = -6.33, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary
############## ARIMA Model without any transformation #####
(arima_model_31 <- arima(ts_international_air,c(0,0,2),seasonal = list(order=c(0,0,2),period=90)))
arima_pred_31 <- predict(arima_model_31, n.ahead = 90)
ts.plot(ts_international_air,arima_pred_31$pred,log="y",lty=c(1,3))
plot(forecast(arima_model_31,h=90))
pred_date_1<- as.Date(as.numeric(time(arima_pred_31$pred)))
pred_international_airfare<- data.frame(pred_date_1,arima_pred_31$pred)
colnames(pred_international_airfare) <- c("Date","MeanNetFare")
View(pred_international_airfare)
###################### END ###############