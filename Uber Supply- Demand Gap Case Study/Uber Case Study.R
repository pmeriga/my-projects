#Loading Data into R#
uber <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)
str(uber)
library(tidyr)
library(dplyr)

## ------------------ Cleaning Uber df -----------------

#####Checking date formats#####

uber$Request.timestamp <- gsub("[/]","-",uber$Request.timestamp)
uber$Request.timestamp <- strptime(uber$Request.timestamp, format = "%d-%m-%Y %H:%M")

uber$Drop.timestamp <- gsub("[/]","-",uber$Drop.timestamp)
uber$Drop.timestamp <- strptime(uber$Drop.timestamp,format = "%d-%m-%Y %H:%M")

#####Checking duplicates#####

sum(duplicated(uber$Request.id)) # no duplicate values

#####Missing values#####
#######Not needed as the NA values are valid#####

#####Checking individual columns#####
summary(uber)

summary(uber$Request.id)

summary(uber$Pickup.point) #Need to change into factor
uber$Pickup.point <- factor(tolower(uber$Pickup.point))
summary(uber$Pickup.point)

summary(uber$Driver.id)

summary(uber$Status) #Need to change into factor
uber$Status <- factor(tolower(uber$Status))
summary(uber$Status)

summary(uber$Request.timestamp)
summary(uber$Drop.timestamp)

summary(uber) #looks good now


uber$Request_hour = format(uber$Request.timestamp, "%H")
summary(uber$Request_hour)
uber$Request_hour <- as.numeric(uber$Request_hour)

uber$slots <-ifelse(uber$Request_hour<= 6, "Wee_Hours",ifelse(uber$Request_hour<= 11,"Busy_Mornings",
ifelse(uber$Request_hour<= 16,"Noon",ifelse(uber$Request_hour<= 19,"Evening_hours", ifelse(uber$Request_hour<= 24,"Night")))))
uber$slots <- factor(uber$slots)
summary((uber$slots))


#####Data Analysis#####
library(ggplot2)

plot1 <- ggplot(uber,aes(x=factor(Request_hour), fill=Status)) + geom_bar(position="dodge") 
plot1 + facet_wrap(~uber$Pickup.point) + ggtitle("Analysing the most pressing problems") +labs(x="Requests per hour", y="Frequency of reqs")

#From plot1, we can identify that overall frequency of requests that get cancelled is more from the City,
#and overall frequency of requests that show 'no cars available' is high from the airport.


plot2 <- ggplot(uber,aes(x=slots, fill=Status)) + geom_bar(position="dodge") 
plot2 + facet_wrap(~uber$Pickup.point) + ggtitle("Requests by Time slots")

#Furthermore, we see that there are 2 most problematic time slots. 
#Problem1. There are no cabs available from the airport in the "Evening_hours" and "Night" slot, i.e, between 4PM to 12 PM. 
#Problem2. There are cancellations happening for the requests from City in the "Busy_mornings" slot, i.e, between 6AM to 11AM.

#Problem1: We will create a plot for the same. 


gap1 <- subset(uber, uber$slots %in% c("Evening_hours","Night"))


plot3 <- ggplot(gap1,aes(x=Status, fill=Pickup.point)) + geom_bar(stat="count",position = "dodge")
plot3 +geom_text(aes(y = round(prop.table(..count..) * 100), label = paste0(round(prop.table(..count..) * 100,digits = 0),'%')), 
position = position_dodge(.9),vjust = .01,stat = 'count') + labs(y = 'Number of requests')


#Calculating the supply and demand
Supply1 <- length(which(gap1$Status=="trip completed" & gap1$Pickup.point=="airport"))
No_supply1 <- length(which(gap1$Status=="no cars available" & gap1$Pickup.point=="airport"))
Demand1 <- length(which(gap1$Pickup.point=="airport"))
pct1 <- (No_supply1/Demand1)*100


#**************************************************************#
  
  
#Problem2: We will create a plot for the same. 

gap2 <- subset(uber, uber$slots=="Busy_Mornings")

plot4 <- ggplot(gap2,aes(x=Status, fill=Pickup.point)) + geom_bar(stat="count",position = "dodge")
plot4 +geom_text(aes(y = round(prop.table(..count..) * 100), label = paste0(round(prop.table(..count..) * 100,digits = 0),'%')), 
position = position_dodge(.4),vjust = .1,stat = 'count') + labs( y = 'number of requests')

#Calculating the supply and demand
Supply2 <- length(which(gap2$Status=="trip completed" & gap2$Pickup.point=="city"))
No_supply2 <- length(which(gap2$Status=="cancelled" & gap2$Pickup.point=="city"))
Demand2 <- length(which(gap2$Pickup.point=="city"))
pct2 <- (No_supply2/Demand2)*100
