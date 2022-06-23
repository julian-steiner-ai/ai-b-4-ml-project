# Read data

setwd('/home/steinerj/Documents/ai-b/semester-4/machine-learning/ai-b-4-ml-project/data')
data <- read.csv("smoking.csv",header=TRUE,sep=",",fill=TRUE,stringsAsFactors=TRUE)

summary(data)

# Remove columnd "ID":
data <- subset(data, select=-ID)

# Create new variable model.data which stores all the variables for training a model

model.data <- data.frame(smoking=data[,"smoking"])

# Analyze and preprocess data

# Smoking variable -> Variable to predict/classify. Convert to a categorical variable.
data[,"smoking"] <- as.factor(data[,"smoking"])

# Gender variable
boxplot(data[,"smoking"] ~ data[,"gender"])

# Contingency boards:
round(table(data[,c("gender","smoking")])/summary(data[,"gender"])*100)

# Include gender variable in machine learning model

model.data <- cbind(model.data, gender=data[,"gender"])

# Age variable

# Plot histogram
hist(data[,"age"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"age"] ~ data[,"smoking"])
boxplot(data[,"age"] ~ data[,"smoking"],outline=FALSE)

# Without outliers and x and y description
boxplot(data[,"age"] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Age")

# Include age variable in machine learning model

model.data <- cbind(model.data, age=data[,"age"])

# Height variable

# Plot histogram
hist(data[,"height.cm."])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"height.cm."] ~ data[,"smoking"])
boxplot(data[,"height.cm."] ~ data[,"smoking"],outline=FALSE)

# Without outliers and x and y description
boxplot(data[,"height.cm."] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Height (cm)")

# Based on the data, it can be seen that taller people smoke more. This variable can be included in the machine learning model.

model.data <- cbind(model.data, height.cm.=data[,"height.cm."])

# Weight variable

# Plot histogram
hist(data[,"weight.kg."])

# Boxplots

par(mfrow=c(1,2))
boxplot(data[,"weight.kg."] ~ data[,"smoking"])
boxplot(data[,"weight.kg."] ~ data[,"smoking"],outline=FALSE)

# Without outliers and x and y description
boxplot(data[,"weight.kg."] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Weight (kg)")

# Based on the data, it can be seen that heavier people smoke more. This variable can be included in the machine learning model.
model.data <- cbind(model.data, weight.kg.=data[,"weight.kg."])

# Waist variable

# Plot histogram
hist(data[,"waist.cm."])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"waist.cm."] ~ data[,"smoking"])
boxplot(data[,"waist.cm."] ~ data[,"smoking"],outline=FALSE)

# Without outliers and x and y description
boxplot(data[,"waist.cm."] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Waist (cm)")

# Eyesight (left & right)

# Show first then datarows
data[1:10,c("eyesight.left.","eyesight.right.")]

# Show all possible values of the variable (left & right)
sort(unique(data[,"eyesight.left."]))
sort(unique(data[,"eyesight.right."]))

# Convert both variables to categorical variables.
data[,"eyesight.left."] <- as.factor(data[,"eyesight.left."])
data[,"eyesight.right."] <- as.factor(data[,"eyesight.right."])

# Boxplots 
par(mfrow=c(1,2))
boxplot(data[,"eyesight.left."] ~ data[,"smoking"])
boxplot(data[,"eyesight.right."] ~ data[,"smoking"])

# Without outliers.
par(mfrow=c(1,2))
boxplot(data[,"eyesight.left."] ~ data[,"smoking"],outline=FALSE)
boxplot(data[,"eyesight.right."] ~ data[,"smoking"],outline=FALSE)

# Show contingency table
round(table(data[,c("eyesight.left.","smoking")])/summary(data[,"eyesight.left."])*100)

# Analze the datarows with eyesight.left. value 1.8 and 1.9.
subset(data,data["eyesight.left."] == 1.8 | data["eyesight.left."] == 1.9)

# This variable does not have a effect on the target variable smokers/non-smokers.
# It is not considered in the machine learning model.

# Hearing variable (left & right)

# Show first ten datarows
data[1:10,c("hearing.left.","hearing.right.")]

# Show summary of this variables 
summary(data[,c("hearing.left.", "hearing.right.")])

# Show all possible values
sort(unique(data[,"hearing.left."]))
sort(unique(data[,"hearing.right."]))

# Convert both variables to boolean variables.
data[,"hearing.left."] <- ifelse(data[,"hearing.left."] == 1, 1, 0)
data[,"hearing.right."] <- ifelse(data[,"hearing.right."] == 1, 1, 0)

data[,"hearing.left."] <- as.factor(data[,"hearing.left."])
data[,"hearing.right."] <- as.factor(data[,"hearing.right."])

# Show summary of this variables 
summary(data[,c("hearing.left.", "hearing.right.")])

# Boxplots 
par(mfrow=c(1,2))
boxplot(data[,"hearing.left."] ~ data[,"smoking"])
boxplot(data[,"hearing.right."] ~ data[,"smoking"])

# Boxplots without outliers
par(mfrow=c(1,2))
boxplot(data[,"hearing.left."] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Hearing Left")
boxplot(data[,"hearing.right."] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Hearing Right")

# Show contingency boards

round(table(data[,c("hearing.left.","smoking")])/summary(data[,"hearing.left."])*100)
round(table(data[,c("hearing.right.","smoking")])/summary(data[,"hearing.right."])*100)

# No real difference on the target variable. Can be omitted for the machine learning model.

# Systolic variable

# Plot histogram
hist(data[,"systolic"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"systolic"] ~ data[,"smoking"])
boxplot(data[,"systolic"] ~ data[,"smoking"],outline=FALSE)

# Small difference in the first quantile of data. Include in the dataset for the machine learning model.

model.data <- cbind(model.data, systolic=data[,"systolic"])

# Relaxation variable

# Plot histogram
hist(data[,"relaxation"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"relaxation"] ~ data[,"smoking"])
boxplot(data[,"relaxation"] ~ data[,"smoking"],outline=FALSE)

# Small difference in the first mean of data.
# Include in the dataset for the machine learning model.

model.data <- cbind(model.data, relaxation=data[,"relaxation"])

# Fasting blood sugar variable

# Plot histogram
hist(data[,"fasting.blood.sugar"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"fasting.blood.sugar"] ~ data[,"smoking"])
boxplot(data[,"fasting.blood.sugar"] ~ data[,"smoking"],outline=FALSE)

# Without outliers and axis description
boxplot(data[,"fasting.blood.sugar"] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Fasting blood sugar")

# Very small difference in all quantiles.
# Include in the dataset for the machine learning model.
model.data <- cbind(model.data, fasting.blood.sugar=data[,"fasting.blood.sugar"])

# Cholesterol variable

# Plot histogram
hist(data[,"Cholesterol"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"Cholesterol"] ~ data[,"smoking"])
boxplot(data[,"Cholesterol"] ~ data[,"smoking"],outline=FALSE)

# Without outliers and axis description
boxplot(data[,"Cholesterol"] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Cholesterol")

# Not a significant differnece. Don't include in the machine learning model.

# Triglyceride variable

# Plot histogram
hist(data[,"triglyceride"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"triglyceride"] ~ data[,"smoking"])
boxplot(data[,"triglyceride"] ~ data[,"smoking"],outline=FALSE)

# Without outliers and axis description
boxplot(data[,"triglyceride"] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Triglyceride")

# Difference in all quantiles.
# Include in the machine learning model.

model.data <- cbind(model.data, triglyceride=data["triglyceride"])

# HDL

# Plot histogram
hist(data[,"HDL"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"HDL"] ~ data[,"smoking"])
boxplot(data[,"HDL"] ~ data[,"smoking"],outline=FALSE)

# Difference in all quantiles.
# Include in the machine learning model.

model.data <- cbind(model.data, HDL=data["HDL"])

# LDL variable

# Plot histogram
hist(data[,"LDL"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"LDL"] ~ data[,"smoking"])
boxplot(data[,"LDL"] ~ data[,"smoking"],outline=FALSE)

# Without outliers and axis description
boxplot(data[,"LDL"] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="LDL")

# Can't see a big difference.
# Don't include in machine learning data.

# Hemoglobin variable

# Plot histogram
hist(data[,"hemoglobin"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"hemoglobin"] ~ data[,"smoking"])
boxplot(data[,"hemoglobin"] ~ data[,"smoking"],outline=FALSE)

# Without outliers and axis description
boxplot(data[,"hemoglobin"] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Hemoglobin")

# Smokers has a higher hemoglobin.
# Include in machine learning model.
model.data <- cbind(model.data, hemoglobin=data["hemoglobin"])

# Urine protein

# Show unique values of this variable
unique(data[,"Urine.protein"])

# Convert as a categorical variable.
data[,"Urine.protein"] <- as.factor(data[,"Urine.protein"])

# Boxplots
par(mfrow=c(1,3))
boxplot(data[,"smoking"] ~ data[,"Urine.protein"])
boxplot(data[,"Urine.protein"] ~ data[,"smoking"])
boxplot(data[,"Urine.protein"] ~ data[,"smoking"],outline=FALSE)

# Contingency boards
round(table(data[,c("Urine.protein","smoking")])/summary(data[,"Urine.protein"])*100)

# The proportion is approximately 59-63% nonsmokers and 37-41% smokers for each protein.
# Exception protein 6. examine more closely:

subset(data,data[,"Urine.protein"]==6)

length(subset(data,data[,"Urine.protein"]==6)[,1])

# These are only 10 datarows of >50000. This are outliers.
# All in all don't include the urine protein variable not in the machine learning model.
# No influence on the target variable

# Serum creatinine variable

# Show all unique values of this variable
sort(unique(data[,"serum.creatinine"]))

# Plot histogram
hist(data[,"serum.creatinine"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"serum.creatinine"] ~ data[,"smoking"])
boxplot(data[,"serum.creatinine"] ~ data[,"smoking"],outline=FALSE)

# Difference in all quantiles. 
# Include in machine learning model.

model.data <- cbind(model.data, serum.creatinine=data[,"serum.creatinine"])

# AST & ALT

# Serum AST level, serum ALT (alanine transaminase) level, and their ratio (AST/ALT ratio) are commonly measured clinically as biomarkers for liver health.
# [Source](https://en.wikipedia.org/wiki/Aspartate_transaminase)
# ALT & AST variable are dependent on each other -> Include only one in the machine learning model. 

# Plot histogram
hist(data[,"AST"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"AST"] ~ data[,"smoking"])
boxplot(data[,"AST"] ~ data[,"smoking"],outline=FALSE)

# Without outliers and axis description
boxplot(data[,"AST"] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="AST")

# Plot histogram
hist(data[,"ALT"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"ALT"] ~ data[,"smoking"])
boxplot(data[,"ALT"] ~ data[,"smoking"],outline=FALSE)

# Without outliers and axis description
boxplot(data[,"ALT"] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="ALT")

# The influence of the ALT variable on the target variable is greater.
# Add this one to the machine learning model.

model.data <- cbind(model.data, ALT=data["ALT"])

# Gtp variable

# Plot histogram
hist(data[,"Gtp"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"Gtp"] ~ data[,"smoking"])
boxplot(data[,"Gtp"] ~ data[,"smoking"],outline=FALSE)

# Without outliers and axis description
boxplot(data[,"Gtp"] ~ data[,"smoking"],outline=FALSE,xlab="Smoking",ylab="Gtp")

# The Gtp of smokers is much greater then from non-smokers
# Include in machine learning model
model.data <- cbind(model.data, Gtp=data["Gtp"])

# Oral variable
summary(data[,"oral"])

# Only "Y" values in the dataset.
# Can be removed in the machine learning model -> Only one value over the whole dataset.

# Dental caries variable

# Summary
summary(data[,"dental.caries"])

# Is a categorical varible
data[,"dental.caries"] <- as.factor(data[,"dental.caries"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"dental.caries"] ~ data[,"smoking"])
boxplot(data[,"dental.caries"] ~ data[,"smoking"],outline=FALSE)

# Contingency boards
round(table(data[,c("dental.caries","smoking")])/summary(data[,"dental.caries"])*100)

# Conclusion: 12% of smokers has more dental caries than non-smokers. Include in machine learning model.
model.data <- cbind(model.data, dental.caries=data["dental.caries"])

# Tartar variable

# Summary
summary(data[,"tartar"])

# Convert to factor
data[,"tartar"] <- as.factor(data[,"tartar"])

# Boxplots
par(mfrow=c(1,2))
boxplot(data[,"tartar"] ~ data[,"smoking"])
boxplot(data[,"tartar"] ~ data[,"smoking"],outline=FALSE)

# Show contingency boards
round(table(data[,c("tartar","smoking")])/summary(data[,"tartar"])*100)

# Conclusion: 10% of smokers has more tartar than non-smokers. Include in machine learning model.
model.data <- cbind(model.data, dental.caries=data["tartar"])

# Summary features for machine learning model

names(model.data)

summary(model.data)