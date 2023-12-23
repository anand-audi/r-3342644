#................................
# *getwd* used to get the data set from the system
#................................
getwd()
library(tidyverse)
library(dplyr)
library(ggplot2)
library(statsr)
#................................
# Loading of the dataset
#................................
project <- readxl::read_xlsx('3342644SportsPeople Data.xlsx')
head(project) # for checking the data

######################################################################################################################################

#...............................
# Q1 - Perform an exploratory data analysis, taking care to describe the type of variables in the data set.
#...............................
summary(data) # Displaying summary  
#...............................
# Creating histograms for BMI and LBM 
# [2]
par(mfrow = c(2, 2))
hist(project$BMI, main = "Histogram of BMI", xlab = "BMI", col = "red")
abline(v=mean(project$BMI), color='c',lwd=3)
text(-10,80,"val")
hist(project$LBM, main = "Histogram of LBM", xlab = "LBM", col = "green")
abline(v=mean(project$LBM), color='c',lwd=3)
#...............................
# Creating boxplots for BMI and LBM 
boxplot(project$BMI, main = "Boxplot of BMI", ylab = "BMI", col = "purple")
boxplot(project$LBM, main = "Boxplot of LBM", ylab = "LBM", col = "skyblue")

######################################################################################################################################

#...............................
# Q2 - Using an appropriate statistical test, investigate whether there is a difference in mean LBM between males and females.
#...............................
# Performing a t-test to compare mean LBM between males and females  
# [3]
glimpse(project)
test_result <- t.test(LBM ~ Sex, data = project) # Perform the t-test
#...............................
# Rounding of the t-test statistics   
test_result$p.value <- round(test_result$p.value, 3)
test_result$estimate <- round(test_result$estimate, 3)
test_result$conf.int <- round(test_result$conf.int, 3)
#...............................
# now printing the rounded results  
test_result

######################################################################################################################################

#...............................
# Q3 - For male and female sports people separately, calculate the correlation coefficient for LBM and BMI given and comment on the relationship between LBM and BMI. Calculating the correlation separately for males and females
#...............................
# [4]
Var_cor_male <- cor(project$LBM[project$Sex == "male"], project$BMI[project$Sex == "male"])
Var_cor_female <- cor(project$LBM[project$Sex == "female"], project$BMI[project$Sex == "female"])
#...............................
#Rounding of the correlation values
Var_cor_male <- round(Var_cor_male, 3)
Var_cor_female <- round(Var_cor_female, 3)
#...............................
#now printing the rounded results
Var_cor_male
Var_cor_female

######################################################################################################################################

#...............................
# Q4 - We would like to investigate a model to test the relationship between LBM and BMI for male sportspeople. You must include output from R to support your findings.
#Details you should include are: 
#...............................
# (a) using your previous results comment on whether there would be any value in including the data for females in this model.
# First lets Summaraise statistics for males
male_data <- subset(project, Sex == "male")
summary(male_data)
# Now Summaraise statistics for females
female_data <- subset(project, Sex == "female")
summary(female_data)
# now comparing the means of LBM and BMI between genders
test_result <- t.test(LBM ~ Sex, data = project)
# Rounding of the t-test statistics
# [5]
test_result$p.value <- round(test_result$p.value, 3)
test_result$estimate <- round(test_result$estimate, 3)
test_result$conf.int <- round(test_result$conf.int, 3)
# now printing the rounded results
test_result
#...............................
# Now comparing the correlations between LBM and BMI for each gender
Var_cor_male <- cor(project$LBM[project$Sex == "male"], project$BMI[project$Sex == "male"])
Var_cor_female <- cor(project$LBM[project$Sex == "female"], project$BMI[project$Sex == "female"])
#Rounding of the correlation values
Var_cor_male <- round(Var_cor_male, 3)
Var_cor_female <- round(Var_cor_female, 3)
#now printing the rounded results for interpretation
Var_cor_male
Var_cor_female
#...............................
# (b) a description of the model;
# Filtering the dataset for male sportspeople
male_data <- subset(project, Sex == "male")
model <- lm(LBM ~ BMI, data = male_data)
#...............................
# (c) a summary of the fitted model with interpretation of test statistics and parameter estimates
summary(model)
#...............................
# (d) conduct a formal test to question whether there is a significant linear relationship between LBM and BMI
model_summary <- summary(model)
plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)
plot(model, which = 5)
# Setting the global option for digits to print three decimal places
options(digits=4)
print(model_summary)
#...........................
# (e) conduct a formal test to question whether there is a significant linear relationship between LBM and BMI.
summary_model <- summary(model)
options(digits=4)
print(summary_model)

######################################################################################################################################

#...............................
# Q5 - Use the model developed in Question 4 to predict the LBM for a male whose BMI is 25.
#...............................
# Fitting the linear model for male data
model <- lm(LBM ~ BMI, data = male_data1)
# Predicting LBM for a male with BMI of 25
Predict_value <- predict(model, newdata = data.frame(BMI = 25))
Predict_value <- round(Predict_value, 3)
Predict_value

######################################################################################################################################

#..........................
# Q6 - Assess the predictive performance of the model.
#..........................
var_residuals <- model$residuals
var_RMSE <- sqrt(mean(var_residuals^2))
R2 <- summary(model)$r.squared
par(mfrow = c(2, 2))
plot(model)

######################################################################################################################################




refrence [1] - https://www.w3schools.com/r/
refrence [2] - https://statisticsglobe.com/add-mean-and-median-to-histogram-in-r
refrence [3] - https://www.datacamp.com/tutorial/t-tests-r-tutorial
refrence [4] - https://www.digitalocean.com/community/tutorials/covariance-and-correlation-in-r-programming
refrence [5] - https://www.geeksforgeeks.org/rounding-off-values-in-r-language-round-function/
