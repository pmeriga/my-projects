######################## HR Case Study #########################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# A company named XYZ, with strength of 4000 employees at any point, is facing a problem with annual attrition of 15%.
# the company has maintained a database containing personal and work related information.

## AIM:

# The aim is to determine what factors mostly influence the attrition in the company.
# Develop a regression model with good accuracy and sensitity to predict the factors influencing the attrition
# Advice HR dept the changes they should make, in order to get most of their employees to stay.


################################################################


### Dataset Understanding:

# There are 5 datasets provided.
# genera_data having 4410 obs and 24 variables, containing the personal and profression background of each employees.
# employee_survey_data having 4410 obs and 4 variables, containing the survey results done by each employees.
# manager_survey_data having 4410 obs and 3 variables, containing the survey results done by manager on each employees.
# in_time having 4410 obs and 262 variables, containing the in time of each of the employees for the year 2015
# out_time having 4410 obs and 262 variables, containing the out time of each of the employees for the year 2015

# all these datasets can be merged upon the EmployeeID column

################################################################

### Data Preparation and EDA:

#set directory
setwd("C:\\Users\\Partha Vijayan\\Downloads\\HR_Analytics - Group Case Study\\PA-I_Case_Study_HR_Analytics")
getwd()

#load libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(e1071)
library(cowplot)
library(stringr)

#load dataset
dataset<- read.csv("general_data.csv", stringsAsFactors = F)
str(dataset)
summary(dataset)

emp_survey<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
str(emp_survey)
summary(emp_survey)

mgr_survey<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
str(mgr_survey)
summary(mgr_survey)

######################## DATA PREPARING ######################## 

#merge datasets
setdiff(dataset$EmployeeID, emp_survey$EmployeeID) #checking the simillarity of emp_id between 2 datasets
setdiff(dataset$EmployeeID, mgr_survey$EmployeeID) #checking the simillarity of emp_id between 2 datasets
identical(dataset$EmployeeID, emp_survey$EmployeeID) #checking the simillarity of emp_id between 2 datasets
identical(dataset$EmployeeID, mgr_survey$EmployeeID) #checking the simillarity of emp_id between 2 datasets

hr_data<- merge(dataset, emp_survey, by = "EmployeeID")
hr_data<- merge(hr_data, mgr_survey, by = "EmployeeID")

View(hr_data)


#load time_in and time_out datasets to check the work_hours for each of the Employees
login_time<- read.csv("in_time.csv", stringsAsFactors = F)
str(login_time)
login_time[,-1]<- lapply(login_time[,-1], function(x) as.POSIXct(x))

logout_time<- read.csv("out_time.csv", stringsAsFactors = F)
str(logout_time)
logout_time[,-1]<- lapply(logout_time[,-1], function(x) as.POSIXct(x))

setdiff(login_time$X, logout_time$X) #both dataset having same EmpID values in the first column X
work_hours<- logout_time[,-1] - login_time[,-1]
str(work_hours)

work_hours<- as.data.frame(sapply(work_hours, function(x) as.numeric(x)))

sapply(work_hours, function(x) length(unique(x))) #there are a few columns having unique as 1
work_hours[which(sapply(work_hours, function(x) length(unique(x))) == 1)] #all the columns having unique value as 1 has NAs
comp_holiday<- sum(sapply(work_hours, function(x) length(unique(x))) == 1)#assuming those columns having all NAs to be comp_holiday

#deriving new columns : avg_workHours, leave_days, no_9hoursMore, no_6hoursLess
work_hours<- work_hours %>% mutate(avg_workHours = rowMeans(work_hours, na.rm = T), 
                                   leave_days = (rowSums(is.na(work_hours)) - comp_holiday))
work_hours<- work_hours %>% mutate(no_9hoursMore = apply(work_hours, 1, function(x) length(which(x > 9))), 
                                   no_6hoursLess = apply(work_hours, 1, function(x) length(which(x < 6))))

work_hours[,262]<- round(work_hours[,262], 0) #rounding off the avg_workHours

# merge datasets to get the master hr_data dataset
hr_data<- cbind(hr_data, work_hours[,262:265])

######################## DATA CLEANING ######################## 

#checking for single value columns
sapply(hr_data, function(x) length(unique(x))) #there are 3 columns, checking to remove them
unique(hr_data$EmployeeCount)
unique(hr_data$Over18)
unique(hr_data$StandardHours)

hr_data<- hr_data[,-c(9, 16, 18)] #removing unwanted columns

#checking for Class and type
sapply(hr_data, class)
sapply(hr_data, typeof)

#missing value imputation
sapply(hr_data, function(x) sum(is.na(x))) #there are 5 columns having NAs
sum(is.na(hr_data))
111/4410*100

# covering a few cases of NAs
compWorked_NA<- hr_data[is.na(hr_data$NumCompaniesWorked),]
compWorked_NA$NumCompaniesWorked<- ifelse(compWorked_NA$YearsAtCompany == compWorked_NA$TotalWorkingYears, 1, NA)
hr_data$NumCompaniesWorked[is.na(hr_data$NumCompaniesWorked)]<-compWorked_NA$NumCompaniesWorked #got 5 NAs fixed
sum(is.na(hr_data$NumCompaniesWorked))

sum(is.na(hr_data))
106/4410*100

#omitting all the NAs row since they are less than 2.5% and cannot be replaced with any value as most of them are survey value miss
hr_data<- na.omit(hr_data)

#outliers treatment
summary(hr_data)

sapply(hr_data[, c(
  "Age",
  "DistanceFromHome",
  "MonthlyIncome",
  "PercentSalaryHike",
  "TotalWorkingYears",
  "YearsAtCompany",
  "YearsSinceLastPromotion",
  "YearsWithCurrManager",
  "avg_workHours",
  "leave_days",
  "no_9hoursMore",
  "no_6hoursLess"
)],
function(x) quantile(x, seq(0, 1, .01)))

#although there are a few columns with outliers, they dont seem to be captured erroneously.
#also to note that a couple of dervied columns has outliers (not removing at this point)

######################## EDA ######################## 
## Categorical variables:
# nomial:
#1) BusinessTravel
#2) Department
#3) EducationField
#4) Gender
#5) JobRole
#6) MaritalStatus

bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position="none")

plot_grid(ggplot(hr_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme, 
          ggplot(hr_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x=Gender,fill=Attrition))+ geom_bar(),
          ggplot(hr_data, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(), align = "h")   

#ordinal (by data description)
#1) Education
#2) EnvironmentSatisfaction
#3) JobInvolvement
#4) JobSatisfaction
#5) PerformanceRating
#6) WorkLifeBalance

plot_grid(ggplot(hr_data, aes(x=Education,fill=Attrition))+ geom_bar(),
          ggplot(hr_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(),
          ggplot(hr_data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(),
          ggplot(hr_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(),
          ggplot(hr_data, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(),
          ggplot(hr_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(), align = "h") 


##Continuous variables
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(hr_data, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr_data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(hr_data, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr_data, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_data, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(hr_data, aes(avg_workHours, fill=Attrition))+ geom_histogram(bins = 5),
          ggplot(hr_data, aes(leave_days, fill=Attrition))+ geom_histogram(bins = 5),
          ggplot(hr_data, aes(no_9hoursMore, fill=Attrition))+ geom_histogram(bins = 5),
          ggplot(hr_data, aes(no_6hoursLess, fill=Attrition))+ geom_histogram(bins = 5),
          align = "v",nrow = 1)

##Derive more variables
#DistanceFromHome
distance<- summary(hr_data$DistanceFromHome)
names(distance)
hr_data$optimalDist<- ifelse(hr_data$DistanceFromHome <= distance[5], 1,0)

#MonthlyIncome
Income<- summary(hr_data$MonthlyIncome)
hr_data$highSalary<- ifelse(hr_data$MonthlyIncome >= Income[4],1,0)

#NumCompaniesWorked and TotalWorkingYears
hr_data$avgYearInComp<- round(ifelse(hr_data$NumCompaniesWorked == 0, hr_data$TotalWorkingYears, 
                                     hr_data$TotalWorkingYears / hr_data$NumCompaniesWorked),digits = 0)

#converting Attrition (target variable) to numeric factors
hr_data$Attrition<- ifelse(hr_data$Attrition == "Yes", 1, 0)

#converting Gender to numerical factors
unique(hr_data$Gender)
hr_data$Gender<- ifelse(hr_data$Gender == "Male" , 1,0)

#creating dummies for the categorical values - one hot coding
dummies_data<- as.data.frame(hr_data[,c(4,5,8,11,12)])
dummies<- data.frame(sapply(dummies_data, function(x) data.frame(model.matrix(~x-1, dummies_data))[,-1]))

#merging dummies to the master dataset
hr_data<- cbind(hr_data[,-c(4,5,8,11,12)], dummies)

#standadise the continuous variables in the master dataset
hr_data_stand<- sapply(hr_data[,c(2, 4, 5, 7:26, 28)], function(x) scale(x))

final_data<- cbind(hr_data[,-c(2, 4, 5, 7:26, 28)], hr_data_stand)
View(final_data)
#################################################################

### Model Building:
set.seed(100)

#split the final data to training and testing datasets
library(caTools)
indices<- sample.split(final_data$Attrition, SplitRatio = 0.7)
train<- final_data[indices,]
test<- final_data[!(indices),]

#removing employeeID variable from train and test
train<- train[,-1]

#building first logistics regression
model1<- glm(Attrition~., data = train, family = "binomial")
summary(model1)
#AIC 2150.4, coeff 45, nullDev 2665.7, resDev 2058.4

#stepwise variable selection
library(MASS)
model2<- stepAIC(model1, direction = "both")

#let's check the multicolinearity 
library(car)
vif(model2)

#buliding model 3 by removing optimalDist
model3<- glm(Attrition ~ BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               EducationField.xMarketing + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle + Age + 
               DistanceFromHome + Education + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               JobInvolvement + avg_workHours + no_6hoursLess, family = "binomial", data = train)

summary(model3)

vif(model3)

#Department.xSales has the high vif but it is also high significant. However removing it to reduce the vif.
model4<- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               EducationField.xMarketing + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle + Age + 
               DistanceFromHome + Education + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               JobInvolvement + avg_workHours + no_6hoursLess, family = "binomial", 
             data = train)

summary(model4)

vif(model4)

#BusinessTravel.xTravel_Frequently has the high vif but it is also high significant. However removing it to reduce the vif.
model5<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               EducationField.xMarketing + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle + Age + 
               DistanceFromHome + Education + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               JobInvolvement + avg_workHours + no_6hoursLess, family = "binomial", 
             data = train)
summary(model5)
vif(model5)


#DistanceFromHome is least significant, so removing it
model6<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               EducationField.xMarketing + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle + Age + 
               Education + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               JobInvolvement + avg_workHours + no_6hoursLess, family = "binomial", 
             data = train)

summary(model6)

vif(model6)

#JobInvolvement is least significant, so removing it
model7<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               EducationField.xMarketing + EducationField.xTechnical.Degree + 
               JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle + Age + 
               Education + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               avg_workHours + no_6hoursLess, family = "binomial", 
             data = train)

summary(model7)
vif(model7)

#JobRole.xHuman.Resources is least significant, so removing it
model8<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               EducationField.xMarketing + EducationField.xTechnical.Degree + 
               JobRole.xManager + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle + Age + 
               Education + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               avg_workHours + no_6hoursLess, family = "binomial", 
             data = train)

summary(model8)
vif(model8)


#JobRole.xResearch.Director is least significant, so removing it
model9<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               EducationField.xMarketing + EducationField.xTechnical.Degree + 
               JobRole.xManager + JobRole.xManufacturing.Director + 
               MaritalStatus.xSingle + Age + 
               Education + NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               avg_workHours + no_6hoursLess, family = "binomial", 
             data = train)

summary(model9)
vif(model9)


#Education is least significant, so removing it
model10<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               EducationField.xMarketing + EducationField.xTechnical.Degree + 
               JobRole.xManager + JobRole.xManufacturing.Director + 
               MaritalStatus.xSingle + Age + 
               NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               avg_workHours + no_6hoursLess, family = "binomial", 
             data = train)

summary(model10)
vif(model10)

#Department.xResearch...Development is least significant, so removing it
model11<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely +  
                EducationField.xMarketing + EducationField.xTechnical.Degree + 
                JobRole.xManager + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + Age + 
                NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                avg_workHours + no_6hoursLess, family = "binomial", 
              data = train)

summary(model11)
vif(model11)


#EducationField.xMarketing is least significant, so removing it
model12<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely +  
                EducationField.xTechnical.Degree + 
                JobRole.xManager + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + Age + 
                NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                avg_workHours + no_6hoursLess, family = "binomial", 
              data = train)

summary(model12)
vif(model12)

#EducationField.xTechnical.Degree is least significant, so removing it
model13<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely +  
                JobRole.xManager + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + Age + 
                NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                avg_workHours + no_6hoursLess, family = "binomial", 
              data = train)

summary(model13)
vif(model13)

#JobRole.xManager is least significant, so removing it
model14<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely +  
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + Age + 
                NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                avg_workHours + no_6hoursLess, family = "binomial", 
              data = train)

summary(model14)
vif(model14)


#TotalWorkingYears has vif = 2.4, so removing it to check if it reduces AIC
model15<- glm(formula = Attrition ~ BusinessTravel.xTravel_Rarely +  
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + Age + 
                NumCompaniesWorked + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                avg_workHours + no_6hoursLess, family = "binomial", 
              data = train)

summary(model15)
vif(model15)
#AIC increases from model14 to model15. so retaining model14 as the final model


#VIF for the variables are less than 2.5 and pvalue's are significant for the 14 variables in the final model
final_model<- model14


################################################################

### Model Evaluation:
# test prediction

test_pred<- predict(final_model, type = "response", newdata = test[,-2])
summary(test_pred)

#adding predicted liklihood to the test dataset
test$predicted_prob<- test_pred

View(test)

#to choose a cutoff, let's begin with 50% to check the accuracy
pred_attrition<- factor(ifelse(test$predicted_prob >= 0.5, "Yes", "No"))
actual_attrition<- factor(ifelse(test$Attrition == 1, "Yes", "No"))

table(actual_attrition, pred_attrition)
accuracy <- (1063+50)/(1063+50+159+20)
sensitivy <-  50/(50+159)
specificity<- 1063/(1063+20)


#since sensitivy is too low, let's try with 45% probablity
pred_attrition<- factor(ifelse(test$predicted_prob >= 0.45, "Yes", "No"))


library(caret)
conf_maxtix<- confusionMatrix(pred_attrition, actual_attrition, positive = "Yes")
conf_maxtix

#sensitivy has improved to .27, but its not better yet

#let's find the optimal cutoff

optimalCutoff<- function(number) {
  pred_attrition<- factor(ifelse(test$predicted_prob >= number, "Yes", "No"))
  conf_maxtix<- confusionMatrix(pred_attrition, actual_attrition, positive = "Yes")
  accuracy<- conf_maxtix$overall[1]
  sensitivy<- conf_maxtix$byClass[1]
  specificity<- conf_maxtix$byClass[2]
  output<- c(accuracy, sensitivy, specificity)
  return(output)
}

#creatingi a cutoff value from 0.0003086 to 0.8454442 for plotting and initializing a random 100x3 matrix
summary(test$predicted_prob)

sequence<- seq(.01, .80, length=100)

matrix<- matrix(0,100,3)

for (i in 1:100) {
  matrix[i,]<- optimalCutoff(sequence[i])
}


plot(sequence, matrix[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(sequence,matrix[,2],col="darkgreen",lwd=2)
lines(sequence,matrix[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff<- seq[which(abs(matrix[,1]-matrix[,2])<0.025)]


#let's choose the cutoff value of 0.1696 for the final model
pred_attrition<- factor(ifelse(test$predicted_prob >= 0.1696, "Yes", "No"))

optimalCutoff(0.1696)
## Accuracy Sensitivity Specificity
## 0.7252322   0.7081340   0.7285319

##################################################################################################
### KS -statistic - Test Data ######

cutoff_attrition<- ifelse(pred_attrition=="Yes", 1, 0)
actual_attrition<- ifelse(actual_attrition=="Yes",1,0)

library(ROCR)
predict_object<- prediction(cutoff_attrition, actual_attrition)
measure_perform<- performance(predict_object, "tpr", "fpr")

ks_table<- attr(measure_perform, "y.values")[[1]]-attr(measure_perform, "x.values")[[1]]

max(ks_table) #decent KS stat value 0.44

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift_helper<- data.frame(cbind(actual_attrition, cutoff_attrition))
lift_helper$bucket<- ntile(-lift_helper[,"cutoff_attrition"], 10) 

attrition_decile<- lift_helper %>% group_by(bucket) %>%
  summarise_at(vars(actual_attrition), funs(total = n(), totalresp = sum(., na.rm = TRUE))) %>% mutate(
    Cumresp = cumsum(totalresp),
    Gain = Cumresp / sum(totalresp) * 100,
    Cumlift = Gain / (bucket * (100 / 10))
  )

#lift chart
plot(attrition_decile$bucket, attrition_decile$Cumlift, type="l", ylab="Cumlift", xlab="Bucket")

#Gain chart
plot(attrition_decile$bucket, attrition_decile$Gain, type="l", ylab="Gain", xlab="Bucket")

####################################################################

### Conclusion
#There are about 14 variables which may influnce an employee attrition in the company.
#1)BusinessTravel.xTravel_Rarely
#2)TotalWorkingYears 
#3)JobRole.xManufacturing.Director
#4)MaritalStatus.xSingle 
#5)Age
#6)NumCompaniesWorked
#7)TrainingTimesLastYear
#8)YearsSinceLastPromotion 
#9)YearsWithCurrManager
#10)EnvironmentSatisfaction 
#11)JobSatisfaction 
#12)WorkLifeBalance
#13)avg_workHours
#14)no_6hoursLess

test_withPred<- cbind(test, pred_attrition)
test_withPred<- test_withPred[,c(1,49)]
dataset_withPred<- merge(dataset, test_withPred, all.y = T)
dataset_withPred<- merge(dataset_withPred, mgr_survey, all.x = T)
dataset_withPred<- merge(dataset_withPred, emp_survey, all.x = T)
dataset_withPred_Yes<- subset(dataset_withPred, dataset_withPred$pred_attrition == "Yes")

#model evaluation output
## Accuracy Sensitivity Specificity
## 0.7252322   0.7081340   0.7285319

#Output: there is a high chance for the employees having the below traits to attrite,
#1) having 10+ years total experience and earning less than 20000
#2) employees reporting to the same manager for more than 4 years and not promoted for more than 4 years
#3) total years of experience more than 20 years, and having mothly income less than 80000 and getting percentSalaryHike lesser than 15%
#4) EnvironmentSatisfaction, JobSatisfaction and WorkLifeBalance having 2 or less values