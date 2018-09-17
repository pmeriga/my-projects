#set working directory
setwd("C://Users/XXXXXX/Downloads")
getwd()

###------------------ Data Sourcing -----------------###
loan_dataset <- read.csv("loan.csv", header = T, sep = ",", stringsAsFactors = FALSE)

#Checking the Data structure
str(loan_dataset)
head(loan_dataset, 10)
summary(loan_dataset)
View(loan_dataset) #since the dataset is huge here


#loading needed libraries to run the EDA
library(stringr)
library(lubridate)
library(ggplot2)
library(zoo);


###------------------ Data Cleaning -----------------###
## Fixing Rows: 
#no Incorrect Header and Footer rows
#no Summary row
#no Extra rows for Page#, column_no
#no blank row

##Fixing Columns: 
#Identify and remove columns with single unique values:
lapply(loan_dataset, function(x) length(unique(x))) #gives count of unique values for all columns
#Removing columns having with single unique value:
length(which(lapply(loan_dataset, function(x) length(unique(x))) == 1)) #gives the count of columns having single value
new_loan_dataset <- loan_dataset[, -which(lapply(loan_dataset, function(x) length(unique(x))) == 1)] #removes the columns having single unique value, (111-60 = 51 columns)

#Identify and remove columns which are not needed for this EDA:
#Removing columns which are 100% unique
length(which(lapply(new_loan_dataset, function(x) length(unique(x))) == 39717)) #gives the count of columns having all values as unique, (51-3 = 48 columns)
new_loan_dataset<- new_loan_dataset[, -which(lapply(new_loan_dataset, function(x) length(unique(x))) == 39717)] #removes the columns having single unique value, (51-3 = 48 columns)
#Removing columns desc, title and zip_code
new_loan_dataset<- new_loan_dataset[, -which(names(new_loan_dataset) %in% c("desc", "title", "zip_code"))] #Reason: desc is the reason for loan application, title is the comment or reference of the loan, and zip_code has incorrect values (or hidden purposely) with which we can't assume
#Renaming earliest_cr_line column name to have consistency in the names for date values
colnames(new_loan_dataset)[colnames(new_loan_dataset) == "earliest_cr_line"] <- "earliest_cr_d"

#Checking the new Data structure
str(new_loan_dataset)
head(new_loan_dataset, 10)
summary(new_loan_dataset)
View(new_loan_dataset) #since the dataset is huge here

#Check for NA values existance on any of the columns
colnames(new_loan_dataset)[apply(is.na(new_loan_dataset), 2, any)]
#[1] "mths_since_last_delinq"     "mths_since_last_record"     "collections_12_mths_ex_med" "chargeoff_within_12_mths"  
#[5] "pub_rec_bankruptcies"       "tax_liens" 
sum(is.na(new_loan_dataset$mths_since_last_delinq))
sum(is.na(new_loan_dataset$mths_since_last_record))
sum(is.na(new_loan_dataset$collections_12_mths_ex_med))
sum(is.na(new_loan_dataset$chargeoff_within_12_mths))
sum(is.na(new_loan_dataset$pub_rec_bankruptcies))
sum(is.na(new_loan_dataset$tax_liens))

#Columns collections_12_mths_ex_med, chargeoff_within_12_mths, tax_liens have minimal number of NA, double checking if NA can be replaced
unique(new_loan_dataset$collections_12_mths_ex_med)
unique(new_loan_dataset$chargeoff_within_12_mths)
unique(new_loan_dataset$tax_liens)
#removing all of these 3 columns since unique values are 0 and NA only, and these will not be needed for this EDA
new_loan_dataset<- new_loan_dataset[,-which(names(new_loan_dataset) %in% c("collections_12_mths_ex_med", "chargeoff_within_12_mths", "tax_liens"))]
#Columns mths_since_last_delinq, mths_since_last_record, pub_rec_bankruptcies have more number of NA, double checking if NA can be replaced 
unique(new_loan_dataset$mths_since_last_delinq)
summary(new_loan_dataset$mths_since_last_delinq) #not replacing them with NA, could be critical data column, need Business/Stakeholder consensus
unique(new_loan_dataset$mths_since_last_record)
summary(new_loan_dataset$mths_since_last_record) #not replacing them with NA, could be critical data column, need Business/Stakeholder consensus
unique(new_loan_dataset$pub_rec_bankruptcies)
summary(new_loan_dataset$pub_rec_bankruptcies) #not replacing them with NA, could be critical data column, need Business/Stakeholder consensus

#Check for NaN values existance on any of the columns
names(which(sapply(new_loan_dataset, function(x) sum(is.nan(x))) > 0)) #no NaN value present

#To check whether emp_title column can be removed
length(unique(new_loan_dataset$emp_title))  #There are 28823 unique values
head(sort(table(new_loan_dataset$emp_title), decreasing = T), 30) #keeping this column as we may check to see to use for this EDA


##Standardise Values:
sapply(new_loan_dataset, class) #to check the date type of each of the columns

#loan_amount column is good
#funded_amnt column is good
#funded_amt_inv column is good
#term column - removing months
new_loan_dataset$term<- as.integer(str_remove(new_loan_dataset$term, " months")) #Removing "months" from values  
typeof(new_loan_dataset$term)
colnames(new_loan_dataset)[colnames(new_loan_dataset) == "term"] <- "term_months" #Renaming column name to add units
#int_rate column - removing %
new_loan_dataset$int_rate = as.numeric(gsub("\\%", "", new_loan_dataset$int_rate)) #Removing % from the values
typeof(new_loan_dataset$int_rate)
colnames(new_loan_dataset)[colnames(new_loan_dataset) == "int_rate"] <- "int_rate_%" #Renaming column name to add units
#installment column is good
head(sort(table((new_loan_dataset$installment)), decreasing = T), 10)
#grade column is good
head(sort(table((new_loan_dataset$grade)), decreasing = T), 10)
#sub_grade column is good
head(sort(table((new_loan_dataset$sub_grade)), decreasing = T), 10)
#emp_title column has "" values, otherwise it is good (it is left without removal as we may check to use it for EDA)
head(sort(table((new_loan_dataset$emp_title)), decreasing = T), 10)
#emp_length column - assumed good
#new_loan_dataset$emp_length<- str_sub(new_loan_dataset$emp_length, 1, -6)
#new_loan_dataset$emp_length<- str_replace(new_loan_dataset$emp_length, "< 1", "0") #Assuming and replacing <1 year experience to 0 since unit is years
#new_loan_dataset$emp_length<- str_replace(new_loan_dataset$emp_length, "10\\+ ", "11") #Assuming and replacing 10+ year experience to 11 since unit is years (generalizing 10+ to 11)
table(new_loan_dataset$emp_length) 
#home_ownership column is good
head(sort(table((new_loan_dataset$home_ownership)), decreasing = T), 10)
#annual_inc column is good
#verification_status column is good
head(sort(table((new_loan_dataset$verification_status)), decreasing = T), 10)
#issue_d column has date and it should be standardised
new_loan_dataset$issue_d <- paste("01-",new_loan_dataset$issue_d, sep = "")
new_loan_dataset$issue_d <-as.Date(new_loan_dataset$issue_d, format="%d-%b-%y")
typeof(new_loan_dataset$issue_d)
#loan_status column is good
head(sort(table((new_loan_dataset$loan_status)), decreasing = T), 10)
#purpose column is good
head(sort(table((new_loan_dataset$purpose)), decreasing = T), 10)
#addr_state column is good
head(sort(table((new_loan_dataset$addr_state)), decreasing = T), 30)
#dti column is good
head(sort(table((new_loan_dataset$dti)), decreasing = T), 30)
#delinq_2yr column is good
head(sort(table((new_loan_dataset$delinq_2yr)), decreasing = T), 30)
#earliest_cr_d column should be standardised
new_loan_dataset$earliest_cr_d <- paste("01-",new_loan_dataset$earliest_cr_d, sep = "")
new_loan_dataset$earliest_cr_d <-as.Date(new_loan_dataset$earliest_cr_d, format="%d-%b-%y")
typeof(new_loan_dataset$earliest_cr_d)
#inq_last_6mths
head(sort(table((new_loan_dataset$inq_last_6mths)), decreasing = T), 30)
#mths_since_last_delinq
head(sort(table((new_loan_dataset$mths_since_last_delinq)), decreasing = T), 30)
#mths_since_last_record
head(sort(table((new_loan_dataset$mths_since_last_record)), decreasing = T), 30)
#open_acc column is good
head(sort(table((new_loan_dataset$open_acc)), decreasing = T), 30)
#pub_rec column is good
head(sort(table((new_loan_dataset$pub_rec)), decreasing = T), 30)
#revol_bal column is good
head(sort(table((new_loan_dataset$revol_bal)), decreasing = T), 30)
#revol_util column is good
new_loan_dataset$revol_util<- as.numeric(str_remove(new_loan_dataset$revol_util, "\\%"))
head(sort(table((new_loan_dataset$revol_util)), decreasing = T), 30)
colnames(new_loan_dataset)[colnames(new_loan_dataset) == "revol_util"]<- "revol_util_%"
#total_acc column is good
head(sort(table((new_loan_dataset$total_acc)), decreasing = T), 30)
#out_prncp column is good
head(sort(table((new_loan_dataset$out_prncp)), decreasing = T), 30)
#out_prncp_inv column is good
head(sort(table((new_loan_dataset$out_prncp_inv)), decreasing = T), 30)
#total_pymnt column is good
head(sort(table((new_loan_dataset$total_pymnt)), decreasing = T), 30)
#total_pymnt_inv column is good
head(sort(table((new_loan_dataset$total_pymnt_inv)), decreasing = T), 30)
#total_rec_prncp column is good
head(sort(table((new_loan_dataset$total_rec_prncp)), decreasing = T), 30)
#total_rec_int column is good
head(sort(table((new_loan_dataset$total_rec_int)), decreasing = T), 30)
#total_rec_late_fee column is good
head(sort(table((new_loan_dataset$total_rec_late_fee)), decreasing = T), 30)
#recoveries column is good
head(sort(table((new_loan_dataset$recoveries)), decreasing = T), 30)
#collection_recovery_fee column is good
head(sort(table((new_loan_dataset$collection_recovery_fee)), decreasing = T), 30)
#last_pymnt_d is a date column and should be standardised
new_loan_dataset$last_pymnt_d <- paste("01-",new_loan_dataset$last_pymnt_d, sep = "")
new_loan_dataset$last_pymnt_d <-as.Date(new_loan_dataset$last_pymnt_d, format="%d-%b-%y")
typeof(new_loan_dataset$last_pymnt_d)
#last_pymnt_amnt column is good
head(sort(table((new_loan_dataset$last_pymnt_amnt)), decreasing = T), 30)
#next_pymnt_d is a date column and should be standardised
new_loan_dataset$next_pymnt_d <- paste("01-",new_loan_dataset$next_pymnt_d, sep = "")
new_loan_dataset$next_pymnt_d <-as.Date(new_loan_dataset$next_pymnt_d, format="%d-%b-%y")
typeof(new_loan_dataset$next_pymnt_d)
#last_credit_pull_d is a date column and should be standardised
new_loan_dataset$last_credit_pull_d <- paste("01-",new_loan_dataset$last_credit_pull_d, sep = "")
new_loan_dataset$last_credit_pull_d <-as.Date(new_loan_dataset$last_credit_pull_d, format="%d-%b-%y")
typeof(new_loan_dataset$last_credit_pull_d)
#pub_rec_bankruptcies column is good
head(sort(table((new_loan_dataset$pub_rec_bankruptcies)), decreasing = T), 30)

##Derived columns: 
#no derived columns required (at this point)


###------------------ Data Manipulation -----------------###
##Creating new dataset containing only ChargedOff loan list
chargedOff_loan<- new_loan_dataset[which(new_loan_dataset$loan_status == "Charged Off"),]
str(chargedOff_loan)
summary(chargedOff_loan)

#Removing not needed columns
lapply(chargedOff_loan, function(x) length(unique(x)))
names(which(lapply(chargedOff_loan, function(x) length(unique(x))) == 1))
#[1] "loan_status"   "out_prncp"     "out_prncp_inv" "next_pymnt_d"
table(chargedOff_loan$loan_status)
table(chargedOff_loan$out_prncp)
table(chargedOff_loan$out_prncp_inv) 
table(chargedOff_loan$next_pymnt_d)
unique(chargedOff_loan$next_pymnt_d)

chargedOff_loan<- chargedOff_loan[, -which(lapply(chargedOff_loan, function(x) length(unique(x))) == 1)] #Removed single value columns
View(chargedOff_loan)

chargedOff_loan<- chargedOff_loan #removing issue_d date column
chargedOff_loan<- chargedOff_loan[,-c(18, 34, 36)] #removing other 3 date columns

chargedOff_loan$emp_title<- str_to_lower(chargedOff_loan$emp_title) #converting case to lower to remove duplicates


###------------------ Data Analysis -----------------###
##Univariate Analysis - unordered categorical
library(plyr)
#emp_title
head(sort(table(chargedOff_loan$emp_title), decreasing = T), 10)
plot(head(sort(table(chargedOff_loan$emp_title), decreasing = T)), type = "l", main = "Job title", frame.plot = T)
#Top 5 companies in which defaulters work with (though it is meant as job title per dictionary, the values mostly look like companies or employers)

#purpose
sort(table(chargedOff_loan$purpose), decreasing = T)
barplot(sort(table(chargedOff_loan$purpose), decreasing = T), main = "Loan Purpose", ylab = "count")
#debt_consolidation purpose clearly shows the higher trend of loan default

#addr_state
sort(table(chargedOff_loan$addr_state), decreasing = T)
barplot(sort(table(chargedOff_loan$addr_state), decreasing = T), main = "customer state", ylab = "count")
#loans from California state has higher change of loan default (however state alone cant be taken as solid condition, this plot is mere awareness purpose only)


##Univariate Analysis - ordered categorical
#emp_length
count(table(chargedOff_loan$emp_length))
plot(table(chargedOff_loan$emp_length), type = "l", main = "Job title")
#customers with 10+ years are the highest defaulters, followed by 2 and 3 years experience customers

#home_ownership
count(table(chargedOff_loan$home_ownership))
plot(table(chargedOff_loan$home_ownership), type = "l", main = "customer home type")
#customers who are in Rent house are the highest defaulters, followed up by customers house which are in Mortgage

#verification_status
count(table(chargedOff_loan$verification_status))
plot(table(chargedOff_loan$verification_status), type = "h", main = "customer home type")
barplot(table(chargedOff_loan$verification_status), main = "customer status", ylab = "count")
#almost both verified and not verified are equal, cannot infer any result

#term_months
table(chargedOff_loan$term_months)
barplot(table(chargedOff_loan$term_months), main = "Term of loan in Months", ylab = "count")
#lower the tenure, higher the risk. 36 months has higher loan default rate

#grade
table(chargedOff_loan$grade)
barplot(table(chargedOff_loan$grade), main = "LC Grade", ylab = "count", col = "blue")
#B grade has the top risk, followed by C grade

#sub_grade
table(chargedOff_loan$sub_grade)
barplot(sort(table(chargedOff_loan$sub_grade)), main = "LC Sub-Grade", ylab = "count", col = "blue")
#B5 grade has the top risk, followed by B3 grade

#delinq_2yrs
table(chargedOff_loan$delinq_2yrs)
barplot(table(chargedOff_loan$delinq_2yrs), main = "delinquency in 2 Years", ylab = "count", col = "pink")
#customers with more deliquency numbers are high risk, delinq_2yrs > 2 can be flagged as risk

#inq_last_6mths
table(chargedOff_loan$inq_last_6mths)
barplot(table(chargedOff_loan$inq_last_6mths), main = "Loan inquired in 6months", ylab = "count", col = "pink")
#customers with more inquiry numbers are high risk, inq_last_6mths > 3 can be flagged as risk

#mths_since_last_delinq
table(chargedOff_loan$mths_since_last_delinq)
barplot(table(chargedOff_loan$mths_since_last_delinq), main = "Customer latest delinquency", ylab = "count", col = "pink")
#customers having last delinquency in less than 24 months can be flagged as high risk customers

#pub_rec_bankruptcies
table(chargedOff_loan$pub_rec_bankruptcies)
barplot(table(chargedOff_loan$pub_rec_bankruptcies), main = "no of bankruptcies filed", ylab = "count", col = "pink")
#customers having >1 should be flagged as high risk customers


##Univariate Analysis - continuous
#loan_amnt
summary(chargedOff_loan$loan_amnt)
hist(chargedOff_loan$loan_amnt, freq = T, labels = T , col = "pink", border = "black", main = "Loan amount")
#no direct inference, though we see the frequency of 5000 is greater

#funded_amnt
summary(chargedOff_loan$funded_amnt)
hist(chargedOff_loan$funded_amnt, freq = T, labels = T , col = "pink", border = "black", main = "Loan funded")
#no direct inference

#funded_amnt_inv
summary(chargedOff_loan$funded_amnt_inv)
hist(chargedOff_loan$funded_amnt_inv, freq = T, labels = T , col = "pink", border = "black", main = "Funded amount - investor commitment")
#no direct inference

#int_rate_%
summary(chargedOff_loan$`int_rate_%`)
hist(chargedOff_loan$`int_rate_%`, freq = T, labels = T , col = "pink", border = "black", main = "Loan interest rate")
boxplot(chargedOff_loan$`int_rate_%`, col = "gray" , range = 0, main = "Loan interest rate")
#there is an higher chance of risk if the interest rate is higher, however we cannot take any inference with this data alone

#installment
summary(chargedOff_loan$installment)
hist(chargedOff_loan$installment, freq = T, labels = T , col = "pink", border = "black", main = "Loan installment")
boxplot(chargedOff_loan$installment, col = "gray" , range = 0, main = "Loan installment")
#no direct inference

#annual_inc
summary(chargedOff_loan$annual_inc)
boxplot(chargedOff_loan$annual_inc, col = "gray" , range = 0, main = "Customer's Annual Income")
#risk of loan default goes high when the customer annual income is close to the Median = 53000

#dti
summary(chargedOff_loan$dti)
boxplot(chargedOff_loan$annual_inc, col = "gray" , range = 0, main = "Ratio of Debt-Total-Income")
#risk of loan default goes high when the customer's dti is close to the Median = 14.29

#mths_since_last_record - ignoring this column for analysis

#open_acc
table(chargedOff_loan$open_acc)
barplot(table(chargedOff_loan$open_acc), main = "No of active accounts Customer holds", ylab = "count", col = "blue")
#no direct inference

#pub_rec
table(chargedOff_loan$pub_rec)
barplot(table(chargedOff_loan$pub_rec), main = "derogatory record", ylab = "count", col = "blue")
# >0 is can be termed as risk customers

#revol_bal
summary(chargedOff_loan$revol_bal)
boxplot(chargedOff_loan$revol_bal, col = "gray" , range = 0, main = "revol_bal")
#no direct inference

#revol_util_% - ignoring this column since revol_bal doesnt have any direct inference

#total_acc
table(chargedOff_loan$total_acc)
barplot(table(chargedOff_loan$total_acc), main = "No of total accounts Customer holds", ylab = "count", col = "blue")
#more number of accounts can be infered as more risky customer

#ignoring total_pymnt, total_pymnt_inv, total_rec_prncp, total_rec_int, total_rec_late_fee, last_pymnt_amnt as these are amounts which would not have direct inference



##Segmented Univariate Analysis
#segmented with home_ownership
chargedOff_loan_Rent<- chargedOff_loan[chargedOff_loan$home_ownership == "RENT",]
chargedOff_loan_Mortgage<- chargedOff_loan[chargedOff_loan$home_ownership == "MORTGAGE",]
chargedOff_loan_Rent_Mortgage<- rbind(chargedOff_loan_Rent, chargedOff_loan_Mortgage)

chargedOff_loan_Own<- chargedOff_loan[chargedOff_loan$home_ownership == "OWN",]
chargedOff_loan_Other<- chargedOff_loan[chargedOff_loan$home_ownership == "OTHER",]
chargedOff_loan_Own_Other<- rbind(chargedOff_loan_Own, chargedOff_loan_Other)

par(mfrow=c(1,2))
summary(chargedOff_loan_Rent_Mortgage$funded_amnt)
summary(chargedOff_loan_Own_Other$funded_amnt)
boxplot(chargedOff_loan_Rent_Mortgage$funded_amnt, col = "pink" , range = 0, main = "Rent_Mortgage - Funded amount")
boxplot(chargedOff_loan_Own_Other$funded_amnt, col = "pink" , range = 0, main = "Own_Other - Funded amount")
#no much difference observed
sum(chargedOff_loan_Rent_Mortgage$funded_amnt)
sum(chargedOff_loan_Own_Other$funded_amnt)
#Risk increases with increase in the funded amount, Rent_Mortgage has way too high of funded amount


par(mfrow=c(1,2))
summary(chargedOff_loan_Rent_Mortgage$delinq_2yrs)
summary(chargedOff_loan_Own_Other$delinq_2yrs)
barplot(table(chargedOff_loan_Rent_Mortgage$delinq_2yrs), main = "Rent_Mortgage- no of delinquency", ylab = "count", col = "blue")
barplot(table(chargedOff_loan_Own_Other$delinq_2yrs), main = "Own_Other- no of delinquency", ylab = "count", col = "blue")
#no of delinquency is lower in Own or Other home_ownership

par(mfrow=c(1,2))
summary(chargedOff_loan_Rent_Mortgage$mths_since_last_delinq)
summary(chargedOff_loan_Own_Other$mths_since_last_delinq)
barplot(table(chargedOff_loan_Rent_Mortgage$mths_since_last_delinq), main = "Rent_Mortgage- last_delinquent_month", ylab = "count", col = "blue")
barplot(table(chargedOff_loan_Own_Other$mths_since_last_delinq), main = "Own_Other- last_delinquent_month", ylab = "count", col = "blue")
#no clear inference observed

par(mfrow=c(1,2))
summary(chargedOff_loan_Rent_Mortgage$`int_rate_%`)
summary(chargedOff_loan_Own_Other$`int_rate_%`)
hist(chargedOff_loan_Rent_Mortgage$`int_rate_%`, main = "Rent_Mortgage- Interest_rate", ylab = "count", col = "blue")
hist(chargedOff_loan_Own_Other$`int_rate_%`, main = "Own_Other- Interest_rate", ylab = "count", col = "blue")
#contribution of frequency is more in Rent_Mortgage on the hight interest rate, which is a clean risk for loan default

par(mfrow=c(1,2))
summary(chargedOff_loan_Rent_Mortgage$grade)
summary(chargedOff_loan_Own_Other$grade)
barplot(table(chargedOff_loan_Rent_Mortgage$grade), main = "Rent_Mortgage- LC_Grade", ylab = "count", col = "blue")
barplot(table(chargedOff_loan_Own_Other$grade), main = "Own_Other- LC_Grade", ylab = "count", col = "blue")
#contribution of risky LC grade B is more in Rent_Mortgage


##Bivariate Analysis
#continuous bivariate - funded_amnt and dti
ggplot(chargedOff_loan, aes(delinq_2yrs, annual_inc ))+geom_line()
#number of delinquency is greater when the annual_inc is lower, they are inversely proposional with respect to the loan default

#categorical bivariate
ggplot(chargedOff_loan, aes(emp_length, fill = home_ownership, col =  verification_status))+geom_bar()
#very evident that Mortgage and Rent home ownership are more as loan defaulters

ggplot(chargedOff_loan, aes(addr_state, fill = emp_length, col =  home_ownership))+geom_bar()
# addr_state level view of the loan defaulters, CA state heads most

ggplot(chargedOff_loan, aes(purpose, funded_amnt))+geom_boxplot()
#by the median - debt_consolidation purpose tops the loan default rate

ggplot(chargedOff_loan, aes(home_ownership, annual_inc))+geom_boxplot()
#by the median - there is a higher chance of loan defaulters if the annual_inc is lesser than 53000

ggplot(chargedOff_loan, aes(`int_rate_%`, fill = purpose))+geom_histogram()
#higher the interest rate, higher the risk. apparently, debt_consolidation has higher interest rate


#Inference:
#the risk of loan default would be high for a customer with the below combinations,
#  addr_state == CA
#  emp_length == 10+
#  home_ownership == RENT | MORTGAGE
#  annual_inc < 53000
#  purpose == debt_consolidation

