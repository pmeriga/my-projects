############################ SVM Hndwritten Digit Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#A classic problem in the field of pattern recognition is that of handwritten digit recognition. 
#The objective is to develop a model using Support Vector Machine which should correctly classify the handwritten digits based on the pixel values given as features.

#####################################################################################

# 2. Data Understanding: 
# We use the MNIST data which is a large database of handwritten digits where we have pixel values of each digit along with its label.  
# Number of Instances: 60,000
# Number of Attributes: 785 

#####################################################################################

#3. Data Preparation: 

#Loading Neccessary libraries

library(ggplot2)
library(dplyr)
library(kernlab)
library(caret)
library(readr)
library(gridExtra)
library(doParallel)


#Loading Training and Test Data

mnist_train_data <- read.csv("mnist_train.csv",header = F)
test <- read.csv("mnist_test.csv",header = F)

#Understanding Dimensions

dim(mnist_train_data) #60000 obs of 785 variables
dim(test) #10000 obs of 785 variables

#Structure of the dataset

str(mnist_train_data)

#printing first few rows

head(mnist_train_data)

#Exploring the data

summary(mnist_train_data)

#checking missing value

sapply(mnist_train_data, function(x) sum(is.na(x))) #No NA values
sapply(test, function(x) sum(is.na(x))) #No NA values

#Making our target class to factor

mnist_train_data$V1 <-factor(mnist_train_data$V1)
summary(mnist_train_data$V1)

test$V1 <-factor(test$V1)
summary(test$V1)

# Split the data into train and test set.
# As the dataset is huge, we are considering only 15% data only.

set.seed(100)
train.indices = sample(1:nrow(mnist_train_data), 0.15*nrow(mnist_train_data))
train = mnist_train_data[train.indices, ]

# Using parallel for faster processing
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

#####################################################################################

#4. Model Building: 

#Constructing Model

####Using Linear Kernel####
Model_linear <- ksvm(V1~ ., data = train, scaled = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$V1) 

#With Linear Model, we are getting an Accuracy : 0.9182


####Using RBF Kernel####

Model_RBF <- ksvm(V1~ ., data = train, scaled = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$V1)

#Using RBF Kernel, we are getting an Accurary : 0.9555. This is the best kernel, we will tune its hyper parameter. 

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 3 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=3)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(100)
grid <- expand.grid(.sigma=c(0.000001, 0.0000015), .C=c(0.5,1,2,3))

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(V1~., data=train, method="svmRadial", metric=metric, tuneGrid=grid1, trControl=trainControl,allowparallel=TRUE)

# Printing cross validation result
print(fit.svm)
# Best tune at sigma = 1e-06  & C=2, Accuracy - 0.9408541

# Plotting model results
plot(fit.svm)

stopCluster(cl) # Stopping parallel processing

######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm, test)
confusionMatrix(evaluate_non_linear, test$V1)

# Accuracy    - 0.9553
# Sensitivity - 0.9857
# Specificity - 0.9977

######################################################################
