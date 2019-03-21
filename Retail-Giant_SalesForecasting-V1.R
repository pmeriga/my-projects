######################## Retail-Giant Sales Forecasting #########################
#################################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#TS Model Building 
#TS Model Evaluation
#################################################################################

### Business Understanding:

# "Global Mart" is an online store super giant having worldwide operations. 
# It takes orders and delivers across the globe and deals with all the major product categories - consumer, corporate & home office.

## AIM:

# As a sales manager, i should forecast the sales and demand for next 6 months to manage the revenue and inventory accordingly.
# The store caters to 7 different market segments and in 3 major categories.  
# the ask is to pick the top 2 profitable (and consistent) market segments for forecasting 


#################################################################################


### Dataset Understanding:

# A dataset of sales of Global Superstore was given, having 51290 obs and 24 variables.
# Having 3 different segments and 7 different markets.

#################################################################################

### Data Preparation and EDA:

#set working directory
setwd("C:\\Users\\Partha Vijayan\\Downloads")
getwd()


#load neeeded libraries
library(dplyr)
library(stringr)
library(tidyr)
library(forecast)
library(tseries)
library(graphics)
library(stats)

#load dataset
sales<- read.csv("Global Superstore.csv", header = T, sep = ",")

dim(sales)
str(sales)
summary(sales) #from the counts (not profit) perspective, segment = Consumer & Corporate and Market = APAC & LATAM are high

View(sales)

# checking for NAs, NULL, NaN - missing value imputation
sapply(sales, function(x) sum(is.na(x))) #Postal.Code has 41296 NA values, however they do not involve in TS modelling, so ignoring
sapply(sales, function(x) sum(is.null(x))) #no NULL values
sapply(sales, function(x) sum(is.nan(x))) #no NaN values
# no data cleaning required

# checking the unique values of the columns
sapply(sales, function(x) length(unique(x)))
# clearly shows Segment has 3 unique values and Market has 7 unique values
unique(sales$Market)
unique(sales$Segment)

# checking the class, typeof of each of the variables
sapply(sales, function(x) class(x)) #note that Order.Date is "factor" and Profit is "numeric"
sapply(sales, function(x) typeof(x)) #note that Order.Date is "integer" and Profit is "double"

# convert Order.Date from integer to date format
sales$Order.Date<-as.Date(sales$Order.Date,"%d-%m-%Y")
typeof(sales$Order.Date)
# not consideringi Ship.Date as we are not going to consider it Market+segment wise aggregation

# deriving month column as the aggregation on data to happen on month wise
library(lubridate)
sales$Order.Month<- month(sales$Order.Date)
sales$Order.Year<- year(sales$Order.Date)

#EDA
barplot(summary(sales$Segment))
barplot(summary(sales$Market))
barplot(table(sales$Segment, sales$Market))
# good view of the composition of the count of orders from each of the Segments and Markets

#For more accurate aggregation, converting months to factoral sequence
sales$Order.Month <- ifelse(sales$Order.Year==2014, sales$Order.Month+36,
                            ifelse(sales$Order.Year==2013, sales$Order.Month+24,
                                   ifelse(sales$Order.Year==2012, sales$Order.Month+12, sales$Order.Month)))

length(unique(sales$Order.Month))

# requirement is to disect the dataset to 7*3 buckets depending on the Segment + Market combinations
sales1<- subset(sales, Market == "US" & Segment == "Consumer")
sales2<- subset(sales, Market == "US" & Segment == "Corporate")
sales3<- subset(sales, Market == "US" & Segment == "Home Office")
sales4<- subset(sales, Market == "APAC" & Segment == "Consumer")
sales5<- subset(sales, Market == "APAC" & Segment == "Corporate")
sales6<- subset(sales, Market == "APAC" & Segment == "Home Office")
sales7<- subset(sales, Market == "EU" & Segment == "Consumer")
sales8<- subset(sales, Market == "EU" & Segment == "Corporate")
sales9<- subset(sales, Market == "EU" & Segment == "Home Office")
sales10<- subset(sales, Market == "Africa" & Segment == "Consumer")
sales11<- subset(sales, Market == "Africa" & Segment == "Corporate")
sales12<- subset(sales, Market == "Africa" & Segment == "Home Office")
sales13<- subset(sales, Market == "EMEA" & Segment == "Consumer")
sales14<- subset(sales, Market == "EMEA" & Segment == "Corporate")
sales15<- subset(sales, Market == "EMEA" & Segment == "Home Office")
sales16<- subset(sales, Market == "LATAM" & Segment == "Consumer")
sales17<- subset(sales, Market == "LATAM" & Segment == "Corporate")
sales18<- subset(sales, Market == "LATAM" & Segment == "Home Office")
sales19<- subset(sales, Market == "Canada" & Segment == "Consumer")
sales20<- subset(sales, Market == "Canada" & Segment == "Corporate")
sales21<- subset(sales, Market == "Canada" & Segment == "Home Office")



#aggregating profit on Order.Month to calc the net profit, sales, qty on month-on-month basis
agg_sales1<- as.data.frame(sales1 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales2<- as.data.frame(sales2 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales3<- as.data.frame(sales3 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales4<- as.data.frame(sales4 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales5<- as.data.frame(sales5 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales6<- as.data.frame(sales6 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales7<- as.data.frame(sales7 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales8<- as.data.frame(sales8 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales9<- as.data.frame(sales9 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales10<- as.data.frame(sales10 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales11<- as.data.frame(sales11 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales12<- as.data.frame(sales12 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales13<- as.data.frame(sales13 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales14<- as.data.frame(sales14 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales15<- as.data.frame(sales15 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales16<- as.data.frame(sales16 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales17<- as.data.frame(sales17 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales18<- as.data.frame(sales18 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales19<- as.data.frame(sales19 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales20<- as.data.frame(sales20 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))
agg_sales21<- as.data.frame(sales21 %>% group_by(Order.Month) %>% summarise(Sales = round(sum(Sales),2), Qty = sum(Quantity), Profit = round(sum(Profit),2)))

# calc CoV of the each of the buckets
CoV_collection <- c(
  CoV1 =  sd(agg_sales1$Profit) / mean(agg_sales1$Profit),
  CoV2 =  sd(agg_sales2$Profit) / mean(agg_sales2$Profit),
  CoV3 = sd(agg_sales3$Profit) / mean(agg_sales3$Profit),
  CoV4 = sd(agg_sales4$Profit) / mean(agg_sales4$Profit),
  CoV5 = sd(agg_sales5$Profit) / mean(agg_sales5$Profit),
  CoV6 = sd(agg_sales6$Profit) / mean(agg_sales6$Profit),
  CoV7 = sd(agg_sales7$Profit) / mean(agg_sales7$Profit),
  CoV8 = sd(agg_sales8$Profit) / mean(agg_sales8$Profit),
  CoV9 = sd(agg_sales9$Profit) / mean(agg_sales9$Profit),
  CoV10 = sd(agg_sales10$Profit) / mean(agg_sales10$Profit),
  CoV11 = sd(agg_sales11$Profit) / mean(agg_sales11$Profit),
  CoV12 = sd(agg_sales12$Profit) / mean(agg_sales12$Profit),
  CoV13 = sd(agg_sales13$Profit) / mean(agg_sales13$Profit),
  CoV14 = sd(agg_sales14$Profit) / mean(agg_sales14$Profit),
  CoV15 = sd(agg_sales15$Profit) / mean(agg_sales15$Profit),
  CoV16 = sd(agg_sales16$Profit) / mean(agg_sales16$Profit),
  CoV17 = sd(agg_sales17$Profit) / mean(agg_sales17$Profit),
  CoV18 = sd(agg_sales18$Profit) / mean(agg_sales18$Profit),
  CoV19 = sd(agg_sales19$Profit) / mean(agg_sales19$Profit),
  CoV20 = sd(agg_sales20$Profit) / mean(agg_sales20$Profit),
  CoV21 = sd(agg_sales21$Profit) / mean(agg_sales21$Profit)
)

# to identify the Top 2 buckets having low CoV
head(sort(CoV_collection), 2)
# it turns out to be CoV4 and CoV7, which are Consumer Segment for APAC and EU Markets

#################################################################################

### TS Modelling1 - forecast sales in APAC Consumer bucket
APAC_Consumer_TS<- ts(agg_sales4$Sales) #to view the overall APAC Consumer sales
plot(APAC_Consumer_TS)

TS_sales1<- as.data.frame(agg_sales4[2])
indata1<- TS_sales1[1:42,]
outdata1<- TS_sales1[43:48,]

timeseries1<- ts(indata1)
plot(timeseries1)

# smoothening of the timeseries1
smoothedseries1 <- stats::filter(timeseries1, 
                                 filter=rep(1/3,3), 
                                 method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries1[3] - smoothedseries1[2]
for (i in seq(1,1,-1)) {
  smoothedseries1[i] <- smoothedseries1[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeseries1)
diff <- smoothedseries1[n-1] - smoothedseries1[n-2]
for (i in seq(n-1+1, n)) {
  smoothedseries1[i] <- smoothedseries1[i-1] + diff
}

#Plot the smoothed time series
lines(smoothedseries1, col="blue", lwd=2) #no sessonalitity observed, so would go ahead with simple linear reg.model


#building a model on the smoothedseries1
month<- agg_sales4[1:42,1]
smoothdf1<- as.data.frame(cbind(month, sales = as.vector(smoothedseries1)))

linear1<- lm(sales~month, data = smoothdf1)
global_pred1<- predict(linear1, data.frame(month))
summary(global_pred1)

#plotting the regression model output
lines(month, global_pred1, col= "red", lwd =2)

#deriving the local predictions
local_pred1<- timeseries1-global_pred1
plot(local_pred1) #plotting the local prediction to observe the stationarity

acf(local_pred1) # clearly not just white noise
pacf(local_pred1) # clearly not just white noise

armafit1<- auto.arima(local_pred1)
tsdiag(armafit1) #clearly shows white noise on top of local pred
armafit1 # AICc=907.78   BIC=909.42, ARIMA(0,0,0)

# checking the residual series
residual1<- local_pred1-fitted(armafit1)


adf.test(residual1,alternative = "stationary")
kpss.test(residual1)

#################################################################################
### Model Evaluation
outdata1
month_out <- agg_sales4[43:48,1]

global_pred_out1 <- predict(linear1,data.frame(Month = month_out))
summary(global_pred_out1)
cbind(outdata1, global_pred_out1)

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec1 <- accuracy(global_pred_out1,outdata1)[5]
MAPE_class_dec1 #MAPE value is 60.91

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred1 <- c(ts(global_pred1),ts(global_pred_out1))
plot(APAC_Consumer_TS , col = "black")
lines(class_dec_pred1, col = "red")

#################################################################################
### TS Modelling2 - forecast sales in EU Consumer bucket
EU_Consumer_TS<- ts(agg_sales7$Sales) #to view the overall EU Consumer sales
plot(EU_Consumer_TS)

TS_sales2<- as.data.frame(agg_sales7[2])
indata2<- TS_sales2[1:42,]
outdata2<- TS_sales2[43:48,]

timeseries2<- ts(indata2)
plot(timeseries2)

# smoothening of the timeseries1
smoothedseries2 <- stats::filter(timeseries2, 
                                 filter=rep(1/3,3), 
                                 method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries2[3] - smoothedseries2[2]
for (i in seq(1,1,-1)) {
  smoothedseries2[i] <- smoothedseries2[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeseries2)
diff <- smoothedseries2[n-1] - smoothedseries2[n-2]
for (i in seq(n-1+1, n)) {
  smoothedseries2[i] <- smoothedseries2[i-1] + diff
}

#Plot the smoothed time series
lines(smoothedseries2, col="blue", lwd=2) #no sessonalitity observed, so would go ahead with simple linear reg.model


#building a model on the smoothedseries1
month<- agg_sales7[1:42,1]
smoothdf2<- as.data.frame(cbind(month, sales = as.vector(smoothedseries2)))

linear2<- lm(sales~month, data = smoothdf2)
global_pred2<- predict(linear2, data.frame(month))
summary(global_pred2)

#plotting the regression model output
lines(month, global_pred2, col= "red", lwd =2)

#deriving the local predictions
local_pred2<- timeseries2-global_pred2
plot(local_pred2) #plotting the local prediction to observe the stationarity

acf(local_pred2) # clearly not just white noise
pacf(local_pred2) # clearly not just white noise

armafit2<- auto.arima(local_pred2)
tsdiag(armafit2) #clearly shows white noise on top of local pred
armafit2 # AICc=908.18   BIC=909.82, ARIMA(0,0,0)

# checking the residual series
residual2<- local_pred2-fitted(armafit2)


adf.test(residual2,alternative = "stationary")
kpss.test(residual2)

#################################################################################
### Model Evaluation
outdata2
month_out <- agg_sales7[43:48,1]

global_pred_out2 <- predict(linear2,data.frame(Month =month_out))
cbind(outdata1, global_pred_out1)

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec2 <- accuracy(global_pred_out2,outdata2)[5]
MAPE_class_dec2 #MAPE value is 58.97

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred2 <- c(ts(global_pred2),ts(global_pred_out2))
plot(EU_Consumer_TS , col = "black")
lines(class_dec_pred2, col = "red")


#################################################################################
### TS Modelling3 - forecast Qty in APAC Consumer bucket
APAC_Consumer_TS_qty<- ts(agg_sales4$Qty) #to view the overall APAC Consumer sales
plot(APAC_Consumer_TS_qty)

TS_sales3<- as.data.frame(agg_sales4[3])
indata3<- TS_sales3[1:42,]
outdata3<- TS_sales3[43:48,]

timeseries3<- ts(indata3)
plot(timeseries3)

# smoothening of the timeseries1
smoothedseries3 <- stats::filter(timeseries3, 
                                 filter=rep(1/3,3), 
                                 method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries3[3] - smoothedseries3[2]
for (i in seq(1,1,-1)) {
  smoothedseries3[i] <- smoothedseries3[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeseries3)
diff <- smoothedseries3[n-1] - smoothedseries3[n-2]
for (i in seq(n-1+1, n)) {
  smoothedseries3[i] <- smoothedseries3[i-1] + diff
}

#Plot the smoothed time series
lines(smoothedseries3, col="blue", lwd=2) #no sessonalitity observed, so would go ahead with simple linear reg.model


#building a model on the smoothedseries1
month<- agg_sales4[1:42,1]
smoothdf3<- as.data.frame(cbind(month, sales = as.vector(smoothedseries3)))

linear3<- lm(sales~month, data = smoothdf3)
global_pred3<- predict(linear3, data.frame(month))
summary(global_pred3)

#plotting the regression model output
lines(month, global_pred3, col= "red", lwd =2)

#deriving the local predictions
local_pred3<- timeseries3-global_pred3
plot(local_pred3) #plotting the local prediction to observe the stationarity

acf(local_pred3) # clearly not just white noise
pacf(local_pred3) # clearly not just white noise

armafit3<- auto.arima(local_pred3)
tsdiag(armafit3) #clearly shows white noise on top of local pred
armafit3 # AICc=531.26   BIC=532.9, ARIMA(0,0,0)

# checking the residual series
residual3<- local_pred3-fitted(armafit3)


adf.test(residual3,alternative = "stationary")
kpss.test(residual3)

#################################################################################
### Model Evaluation
outdata3
month_out <- agg_sales4[43:48,1]

global_pred_out3 <- predict(linear3,data.frame(Month =month_out))
cbind(outdata3, global_pred_out3)

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec3 <- accuracy(global_pred_out3,outdata3)[5]
MAPE_class_dec3 #MAPE value is 60.97

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred3 <- c(ts(global_pred3),ts(global_pred_out3))
plot(APAC_Consumer_TS_qty , col = "black")
lines(class_dec_pred3, col = "red")

#################################################################################
### TS Modelling4 - forecast Qty in EU Consumer bucket
EU_Consumer_TS_qty<- ts(agg_sales7$Qty) #to view the overall EU Consumer sales
plot(EU_Consumer_TS_qty)

TS_sales4<- as.data.frame(agg_sales7[3])
indata4<- TS_sales4[1:42,]
outdata4<- TS_sales4[43:48,]

timeseries4<- ts(indata4)
plot(timeseries4)

# smoothening of the timeseries1
smoothedseries4 <- stats::filter(timeseries4, 
                                 filter=rep(1/3,3), 
                                 method='convolution', sides=2)

#Smoothing left end of the time series
diff <- smoothedseries4[3] - smoothedseries4[2]
for (i in seq(1,1,-1)) {
  smoothedseries4[i] <- smoothedseries4[i+1] - diff
}

#Smoothing right end of the time series
n <- length(timeseries4)
diff <- smoothedseries4[n-1] - smoothedseries4[n-2]
for (i in seq(n-1+1, n)) {
  smoothedseries4[i] <- smoothedseries4[i-1] + diff
}

#Plot the smoothed time series
lines(smoothedseries4, col="blue", lwd=2) #no sessonalitity observed, so would go ahead with simple linear reg.model


#building a model on the smoothedseries1
month<- agg_sales7[1:42,1]
smoothdf4<- as.data.frame(cbind(month, sales = as.vector(smoothedseries4)))

linear4<- lm(sales~month, data = smoothdf4)
global_pred4<- predict(linear4, data.frame(month))
summary(global_pred4)

#plotting the regression model output
lines(month, global_pred4, col= "red", lwd =2)

#deriving the local predictions
local_pred4<- timeseries4-global_pred4
plot(local_pred4) #plotting the local prediction to observe the stationarity

acf(local_pred4) # clearly not just white noise
pacf(local_pred4) # clearly not just white noise

armafit4<- auto.arima(local_pred4)
tsdiag(armafit4) #clearly shows white noise on top of local pred
armafit4 # AICc=534.28   BIC=535.92, ARIMA(0,0,0)

# checking the residual series
residual4<- local_pred4-fitted(armafit4)


adf.test(residual4,alternative = "stationary")
kpss.test(residual4)

#################################################################################
### Model Evaluation
outdata4
month_out <- agg_sales7[43:48,1]

global_pred_out4 <- predict(linear4,data.frame(Month =month_out))
cbind(outdata4, global_pred_out4)

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec4 <- accuracy(global_pred_out4,outdata4)[5]
MAPE_class_dec4 #MAPE value is 61.67

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred4 <- c(ts(global_pred4),ts(global_pred_out4))
plot(EU_Consumer_TS_qty , col = "black")
lines(class_dec_pred4, col = "red")
#################################################################################

#################################################################################


#Conclusion
# 4 TS models were created to forecast the sales and quantity of APAC consumer and EU consumer buckets with the below MAPE values respectively,
# (visual projections are in the coding part)

#model1   APAC_Consumer_Sales     MAPE == 60.91
#model2   EU_Consumer_Sales   MAPE == 58.97
#model3   APAC_Consumer_Qty       MAPE == 60.97
#model2   EU_Consumer_Qty     MAPE == 61.67

#################################################################################

