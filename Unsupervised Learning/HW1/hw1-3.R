rm(list = ls())
graphics.off()
library("arules")
library("MASS")
library("rpart")
library("ElemStatLearn")
#Tried getting dataset from library, unsuccessful. Therefore changing working directory where the dataset is downloaded
setwd("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining II/Code")
load("marketing.RData")
head(marketing)

######## To generate a reference sample from the training set
### Using sample function for all 14 variables of marketing dataset
### x = range of values the variables must take
### size = 8993 because reference sample ko original ke same size ka banana hai
### replace = TRUE everytime because no element must occur twice

Income = sample(seq(1,9), 8993, replace = TRUE) 
Sex = sample(c(1,2), 8993,replace = TRUE) 
Marital = sample(seq(1,5), 8993, replace = TRUE)
Age = sample(seq(1,7), 8993, replace = TRUE)
Edu = sample(seq(1,6), 8993, replace = TRUE)
Occupation = sample(seq(1,9), 8993, replace = TRUE)
Lived = sample(seq(1,5), 8993, replace = TRUE)
Dual_Income = sample(seq(1,3), 8993, replace = TRUE)
Household = sample(seq(1,9), 8993, replace = TRUE)
Householdu18 = sample(seq(1,9), 8993, replace = TRUE)
Status = sample(seq(1,3), 8993, replace = TRUE)
Home_Type = sample(seq(1,5), 8993, replace = TRUE)
Ethnic = sample(seq(1,8), 8993, replace = TRUE) 
Language = sample(seq(1,3), 8993, replace = TRUE)

######## Making a data frame from all the variables
training = data.frame(Income, Sex, Marital, Age, Edu, Occupation, Lived, Dual_Income, Household, 
                             Householdu18, Status, Home_Type, Ethnic, Language)
######## Naming the variables same as the marketing dataset
names(training) = c("Sex", "Martial_Status", "Age", "Education", "Occupation", "Income", "Years_In_BayArea", "Dual_Incomes", "Numbers_in_Household", 
                           "Number_of_Children", "Householder_Status", "Type_of_Home", "Ethinic_Classification", "Language_in_Home")
###########################
########This is our class 1
training[["target"]] = 1 
reference = training
for(i in 1:ncol(reference)){
  reference[,i] = sample(reference[,i], nrow(reference), replace = FALSE)
}

###########################
########This is our class 0
reference[["target"]] = 0

###########################
########Combining the data
combined = rbind(reference, training);
model.controls <- rpart.control(minbucket = 2, minsplit = 4, xval = 10, cp = 0.01)
model <- rpart(target~., data = combined, method = "class", control = model.controls)
summary(model)
plot(model)
