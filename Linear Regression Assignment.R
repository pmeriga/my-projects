#Loading Data into R#

carprice <- read.csv("carprice_assignment.csv")

View(carprice)
str(carprice)

## ------------------ Cleaning Carprice df ----------------- ##

#####Changing variables to Factor Type#####
carprice$symboling <- as.factor(carprice$symboling)
carprice$peakrpm <- as.factor(carprice$peakrpm)
carprice$citympg <- as.factor(carprice$citympg)
carprice$highwaympg <- as.factor(carprice$highwaympg)

#Checking the structure again#
str(carprice)

#####Checking duplicates#####
unique(carprice)

##check for missing values and treat if any##
sum(is.na(carprice))

#Data Preparation Steps#

#Separating Car Company Name as mentioned in the Guidelines#
#Loading the libraries
library(tidyr)
library(dplyr)

carprice <- separate(carprice,CarName,into=c("car_company","car_model"),sep=" ",extra="merge")
str(carprice$car_company)


#Checking the distinct Car companies#
distinct(carprice,car_company)
carprice$car_company[carprice$car_company=="maxda"] <- "mazda"
carprice$car_company[carprice$car_company=="Nissan"] <- "nissan"
carprice$car_company[carprice$car_company=="porcshce"] <- "porsche"
carprice$car_company[carprice$car_company=="toyouta"] <- "toyota"
carprice$car_company[carprice$car_company=="vokswagen"] <- "volkswagen"
carprice$car_company[carprice$car_company=="vw"] <- "volkswagen"
carprice$car_company <- as.factor(carprice$car_company)
str(carprice$car_company)

#Removing Car Model and Car ID variables as it is not necessary for analysis
carprice <- carprice[,-1]
carprice <- carprice[,-3]


str(carprice)

##############DUMMY VARIABLE CREATION##########

#Converting categorical factor variables into numeric
levels(carprice$fueltype)<-c(1,0)
carprice$fueltype <- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

levels(carprice$aspiration)<-c(1,0)
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]

levels(carprice$doornumber)<-c(1,0)
carprice$doornumber <- as.numeric(levels(carprice$doornumber))[carprice$doornumber]

levels(carprice$enginelocation)<-c(1,0)
carprice$enginelocation <- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

str(carprice)

# Now we come across categorical and numeric variables having more than 3 levels.
#Creating new levels by binning the numeric levels
summary(carprice$symboling)
levels(carprice$symboling)[1:3] <- "less_risky"
levels(carprice$symboling)[2:4] <- "high_risky"
dummy1<-model.matrix(~symboling - 1,data=carprice)
summary(dummy1)
# Combine the dummy variables to the main data set, after removing the original categorical column
which( colnames(carprice)=="symboling")
carprice_1<- cbind(carprice[,-1], dummy1)

str(carprice_1)

#Checking and creating more dummy variables
summary(factor(carprice_1$car_company))
dummy2 <- (model.matrix( ~car_company -1, data = carprice_1))
summary(dummy2)
which( colnames(carprice_1)=="car_company")
carprice_1 <- cbind(carprice_1[,-1], dummy2)

str(carprice_1)

summary(factor(carprice_1$carbody))
dummy3 <- (model.matrix( ~carbody -1, data = carprice_1))
summary(dummy3)
which( colnames(carprice_1)=="carbody" )
carprice_1 <- cbind(carprice_1[,-4], dummy3)

str(carprice_1)

summary(factor(carprice_1$drivewheel))
dummy4 <- (model.matrix( ~drivewheel -1, data = carprice_1))
summary(dummy4)
which( colnames(carprice_1)=="drivewheel" )
carprice_1 <- cbind(carprice_1[,-4], dummy3)

str(carprice_1)

summary(factor(carprice_1$cylindernumber))
dummy5 <- (model.matrix( ~cylindernumber -1, data = carprice_1))
summary(dummy5)
which( colnames(carprice_1)=="cylindernumber" )
carprice_1 <- cbind(carprice_1[,-11], dummy5)

str(carprice_1)

summary(factor(carprice_1$enginetype))
dummy6 <- (model.matrix( ~enginetype -1, data = carprice_1))
summary(dummy6)
which( colnames(carprice_1)=="enginetype" )
carprice_1 <- cbind(carprice_1[,-10], dummy6)

str(carprice_1)

summary(factor(carprice_1$fuelsystem))
dummy7 <- (model.matrix( ~fuelsystem -1, data = carprice_1))
summary(dummy7)
which( colnames(carprice_1)=="fuelsystem" )
carprice_1 <- cbind(carprice_1[,-11], dummy7)

str(carprice_1)

summary(carprice_1$peakrpm)
levels(carprice_1$peakrpm)[1:6] <- "less_rpm"
levels(carprice_1$peakrpm)[2:6] <- "medium_rpm"
levels(carprice_1$peakrpm)[3:8] <- "good_rpm"
levels(carprice_1$peakrpm)[4:9] <- "high_rpm"
dummy8<-model.matrix(~peakrpm - 1,data=carprice_1)
summary(dummy8)
which( colnames(carprice_1)=="peakrpm" )
carprice_1<- cbind(carprice_1[,-15], dummy8)

str(carprice_1)

summary(carprice_1$citympg)
levels(carprice_1$citympg)[1:8] <- "less_mpg"
levels(carprice_1$citympg)[2:11] <- "good_mpg"
levels(carprice_1$citympg)[3:13] <- "high_mpg"
dummy9<-model.matrix(~citympg - 1,data=carprice_1)
summary(dummy9)
which( colnames(carprice_1)=="citympg" )
carprice_1<- cbind(carprice_1[,-15], dummy9)

str(carprice_1)

summary(carprice_1$highwaympg)
levels(carprice_1$highwaympg)[1:9] <- "less_mpg"
levels(carprice_1$highwaympg)[2:10] <- "good_mpg"
levels(carprice_1$highwaympg)[3:14] <- "high_mpg"
dummy10<-model.matrix(~highwaympg - 1,data=carprice_1)
summary(dummy10)
which( colnames(carprice_1)=="highwaympg" )
carprice_1<- cbind(carprice_1[,-15], dummy10)

str(carprice_1)

#We created total 10 dummy varaibles!

#Next step is to treat the outliers.
#We are looking for quantile values at each 1% interval and wherever there is a high jump from one quantile to another, we cap/floor those values
quantile(carprice_1$wheelbase,seq(0,1,0.01))
carprice_1$wheelbase[which(carprice_1$wheelbase>115.544)]<-115.544

quantile(carprice_1$carlength,seq(0,1,0.01))
carprice_1$carlength[which(carprice_1$carlength>192.700)]<-192.700

#No sudden jump in the quantile values indicates there are no outliers in that column
#looks ok
quantile(carprice_1$carwidth,seq(0,1,0.01))

#looks ok
quantile(carprice_1$carheight,seq(0,1,0.01))

#looks ok
quantile(carprice_1$curbweight,seq(0,1,0.01))

#We see huge jump from 94%. Hence flooring values at 94%
quantile(carprice_1$enginesize,seq(0,1,0.01))
carprice_1$enginesize[which(carprice_1$enginesize>194.00)]<-194.00

#looks ok
quantile(carprice_1$boreratio,seq(0,1,0.01))

#looks ok
quantile(carprice_1$stroke,seq(0,1,0.01))

#looks ok
quantile(carprice_1$compressionratio,seq(0,1,0.01))

#Although there is sudden jump between 99% and 100%, since it is horsepower, it might be correct. Hence not changing it.
quantile(carprice_1$horsepower,seq(0,1,0.01))


###########MODEL BUILDING#######################
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(carprice_1), 0.7*nrow(carprice_1))
train = carprice_1[trainindices,]
test = carprice_1[-trainindices,]


# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)
#######

# Now, lets use stepAIC to build next model
#Loading the libraries
#install.packages("MASS")
library(MASS)
#install.packages("car")
library(car)

step <- stepAIC(model_1, direction="both")

step

#Removing the variables we identified from stepAIC output, and executing next model 
model_2 <-lm(price ~ fueltype + aspiration + enginelocation + carwidth + carheight + 
               curbweight + boreratio + stroke + compressionratio + `car_companyalfa-romero` + 
               car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
               car_companyjaguar + car_companypeugeot + car_companysubaru + 
               carbodyconvertible + carbodyhardtop + carbodyhatchback + 
               carbodysedan + cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + peakrpmmedium_rpm + citympgless_mpg + 
               citympggood_mpg + highwaympgless_mpg, data=train)

summary(model_2)

vif(model_2)

#Removing citympgless_mpg as it has high p-value, high vif and execute next model
model_3 <-lm(price ~ fueltype + aspiration + enginelocation + carwidth + carheight + 
               curbweight + boreratio + stroke + compressionratio + `car_companyalfa-romero` + 
               car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
               car_companyjaguar + car_companypeugeot + car_companysubaru + 
               carbodyconvertible + carbodyhardtop + carbodyhatchback + 
               carbodysedan + cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + peakrpmmedium_rpm + 
               citympggood_mpg + highwaympgless_mpg, data=train)


summary(model_3)
vif(model_3)

#Removing highwaympgless_mpg as it has high p-value, high vif and execute next model
model_3.1 <-lm(price ~ fueltype + aspiration + enginelocation + carwidth + carheight + 
                 curbweight + boreratio + stroke + compressionratio + `car_companyalfa-romero` + 
                 car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
                 car_companyjaguar + car_companypeugeot + car_companysubaru + 
                 carbodyconvertible + carbodyhardtop + carbodyhatchback + 
                 carbodysedan + cylindernumbereight + cylindernumberfive + 
                 cylindernumberfour + peakrpmmedium_rpm + 
                 citympggood_mpg , data=train)


summary(model_3.1)
vif(model_3.1)


#Removing fueltype as it has high p-value, high vif and execute next model
model_4 <-lm(price ~  aspiration + enginelocation + carwidth + carheight + 
               curbweight + boreratio + stroke + compressionratio + `car_companyalfa-romero` + 
               car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
               car_companyjaguar + car_companypeugeot + car_companysubaru + 
               carbodyconvertible + carbodyhardtop + carbodyhatchback + 
               carbodysedan + cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + peakrpmmedium_rpm + 
               citympggood_mpg , data=train)


summary(model_4)
vif(model_4)

#Removing compressionratio as it has higher p-value and execute next model
model_5 <-lm(price ~  aspiration + enginelocation + carwidth + carheight + 
               curbweight + boreratio + stroke + `car_companyalfa-romero` + 
               car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
               car_companyjaguar + car_companypeugeot + car_companysubaru + 
               carbodyconvertible + carbodyhardtop + carbodyhatchback + 
               carbodysedan + cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + peakrpmmedium_rpm + 
               citympggood_mpg, data=train)


summary(model_5)
vif(model_5)

#Removing carbodyhatchback as it has higher p-value and execute next model
model_6 <-lm(price ~  aspiration + enginelocation + carwidth + carheight + 
               curbweight + boreratio + stroke + `car_companyalfa-romero` + 
               car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
               car_companyjaguar + car_companypeugeot + car_companysubaru + 
               carbodyconvertible + carbodyhardtop +
               carbodysedan + cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + peakrpmmedium_rpm + 
               citympggood_mpg, data=train)


summary(model_6)
vif(model_6)

#Removing carbodyhardtop as it has higher p-value and execute next model
model_7 <-lm(price ~  aspiration + enginelocation + carwidth + carheight + 
               curbweight + boreratio + stroke + `car_companyalfa-romero` + 
               car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
               car_companyjaguar + car_companypeugeot + car_companysubaru + 
               carbodyconvertible  +
               carbodysedan + cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + peakrpmmedium_rpm + 
               citympggood_mpg, data=train)


summary(model_7)
vif(model_7)

#Removing carbodysedan as it has higher p-value and execute next model
model_8 <-lm(price ~  aspiration + enginelocation + carwidth + carheight + 
               curbweight + boreratio + stroke + `car_companyalfa-romero` + 
               car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
               car_companyjaguar + car_companypeugeot + car_companysubaru + 
               carbodyconvertible + cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + peakrpmmedium_rpm + 
               citympggood_mpg, data=train)


summary(model_8)
vif(model_8)

#Removing carbodyconvertible as it has higher p-value and execute next model
model_9 <-lm(price ~  aspiration + enginelocation + carwidth + carheight + 
               curbweight + boreratio + stroke + `car_companyalfa-romero` + 
               car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
               car_companyjaguar + car_companypeugeot + car_companysubaru + 
               cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + peakrpmmedium_rpm + 
               citympggood_mpg, data=train)


summary(model_9)
vif(model_9)

#Removing carheight as it has higher p-value and execute next model
model_10 <-lm(price ~  aspiration + enginelocation + carwidth +
               curbweight + boreratio + stroke + `car_companyalfa-romero` + 
               car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
               car_companyjaguar + car_companypeugeot + car_companysubaru + 
               cylindernumbereight + cylindernumberfive + 
               cylindernumberfour + peakrpmmedium_rpm + 
               citympggood_mpg, data=train)


summary(model_10)
vif(model_10)

#Removing citympggood_mpg as it has higher p-value and execute next model
model_11 <-lm(price ~  aspiration + enginelocation + carwidth +
                curbweight + boreratio + stroke + `car_companyalfa-romero` + 
                car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
                car_companyjaguar + car_companypeugeot + car_companysubaru + 
                cylindernumbereight + cylindernumberfive + 
                cylindernumberfour + peakrpmmedium_rpm , data=train)


summary(model_11)
vif(model_11)

#Removing cylindernumberfive as it has higher p-value and execute next model
model_12 <-lm(price ~  aspiration + enginelocation + carwidth +
                curbweight + boreratio + stroke + `car_companyalfa-romero` + 
                car_companyaudi + car_companybmw + car_companybuick + car_companyhonda + 
                car_companyjaguar + car_companypeugeot + car_companysubaru + 
                cylindernumbereight + cylindernumberfour + peakrpmmedium_rpm , data=train)


summary(model_12)
vif(model_12)

#Removing car_companyaudi as it has higher p-value and execute next model
model_13 <-lm(price ~  aspiration + enginelocation + carwidth +
                curbweight + boreratio + stroke + `car_companyalfa-romero` + 
                 car_companybmw + car_companybuick + car_companyhonda + 
                car_companyjaguar + car_companypeugeot + car_companysubaru + 
                cylindernumbereight + cylindernumberfour + peakrpmmedium_rpm , data=train)


summary(model_13)
vif(model_13)

#Removing car_companyhonda as it has higher p-value and execute next model
model_14 <-lm(price ~  aspiration + enginelocation + carwidth +
                curbweight + boreratio + stroke + `car_companyalfa-romero` + 
                car_companybmw + car_companybuick + 
                car_companyjaguar + car_companypeugeot + car_companysubaru + 
                cylindernumbereight + cylindernumberfour + peakrpmmedium_rpm , data=train)


summary(model_14)
vif(model_14)

#Removing boreratio as it has higher p-value and execute next model
model_15 <-lm(price ~  aspiration + enginelocation + carwidth +
                curbweight + stroke + `car_companyalfa-romero` + 
                car_companybmw + car_companybuick + 
                car_companyjaguar + car_companypeugeot + car_companysubaru + 
                cylindernumbereight + cylindernumberfour + peakrpmmedium_rpm , data=train)


summary(model_15)
vif(model_15)

#We arrived at the best model. Hence the variables in model_15 are the appropriate ones to understand  the pricing dynamics##


# predicting the results in test dataset
which( colnames(test)=="price" )
Predict_1 <- predict(model_15,test[,-15])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- r^2
rsquared


#Hence the final Adjusted R-squared:  0.9621 and rsquared is 0.8573608