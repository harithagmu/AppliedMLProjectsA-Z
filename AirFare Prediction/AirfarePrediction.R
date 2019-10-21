# Prediction of AirFares - Linear Regression Models

# Set working directory to working file location
setwd("D:\\SPRING 2019\\OR 568\\HomeWorks")   

# Read the csv file
AFData<-read.csv("AirfaresData.csv",header=TRUE)
head(AFData)
View(AFData)
nrow(AFData)
#Understanding the datatypes of columns
str(AFData)
# NUMERICAL predictors in the dataset are COUPON, NEW, HI, S_INCOME, E_INCOME, S_POP, E_POP, DISTANCE, PAX
#Creating a subset of dataset for numerical predictors
mynumvars = c(5,6,9:13,16:18)
AFNumData = AFData[mynumvars]
head(AFNumData)
#Exploring the relation between the  numerical predictors and the response variable.
plot(AFNumData)
View(cor(AFNumData))
# Individual Scatterplots
plot(AFNumData$DISTANCE, AFNumData$FARE, main="Fare vs. Distance", xlab="Distance", ylab="Fare")
plot(AFNumData$COUPON, AFNumData$FARE, main="Fare vs. Coupons", xlab="Coupon", ylab="Fare")
plot(AFNumData$S_INCOME, AFNumData$FARE, main="FARE vs. S_INCOME", xlab="Starting city's population", ylab="Fare")
plot(AFNumData$E_INCOME, AFNumData$FARE, main="FARE vs. E_INCOME", xlab="Ending city's population", ylab="Fare")

#Explore the categorical predictors computing the mean value of FARE according to each category.
summaryVacation <-aggregate(FARE~VACATION, data = AFData, FUN = mean)
summarySW <-aggregate(FARE~SW, data = AFData, FUN = mean)
summarySlot <-aggregate(FARE~SLOT, data = AFData, FUN = mean)
summaryGate <-aggregate(FARE~GATE, data = AFData, FUN = mean)
#Find the categorical predictor that has the largest difference in mean FARE values between qualitative levels
AFCatData = AFData[c(7,8,14,15,18)]
head(AFCatData)
t.test(FARE~VACATION, data = AFData )
t.test(FARE~SW, data = AFData )
t.test(FARE~SLOT, data = AFData )
t.test(FARE~GATE, data = AFData )

#Model building
# Subsetting the dataset into training and validation 
set.seed(12345)
trainindex <- sample(row, 600, replace=FALSE)
training <- AFData[trainindex, ]
validation <- AFData[-trainindex, ]

#Model building with the 2 best predictors identified from above steps
fitModel <- lm(FARE ~ DISTANCE + SW, data = training)

# Summary of the regression
summary(fitModel)
AIC(fitModel)
BIC(fitModel)
fitsummary = summary(fitModel)
fitsummary$r.squared
fitsummary$adj.r.squared
# Store model residuals in a vector and plot their histogram with 20 bars 
residual<-residuals(fitModel)
plot(residual)
hist(residual,breaks=20)
# QQ plot of residual
qqnorm(residual, ylab="Standardized Residuals", xlab="Normal Scores", main="Residual") 
qqline(residual)
#Residual vs predicted values
layout(matrix(c(1,2,3,4),2,2))           # optional 4 graphs/page 
plot(fitModel)

# Computing the MSE on validation dataset based on model fit with training data
# We use the "predict" function to compute the predicted value on validation set
PredBase<-predict(fitModel, validation, se.fit=TRUE)  
PredBase
mean((validation[,"FARE"] - PredBase$fit)^2)

#Backward selection 
#Constructing a full model
fullModel <- lm(FARE ~ COUPON + NEW + VACATION + SW + HI + S_INCOME + E_INCOME + S_POP + E_POP + SLOT + GATE + DISTANCE + PAX, data = training)
summary.full <- summary(fullModel)
summary.full$r.squared
PredFull<-predict(fullModel, validation, se.fit=TRUE)  
PredFull
mean((validation[,"FARE"] - PredFull$fit)^2)

#From the full model to find the most significant predictors using backward selection
backward<-step(fullModel, direction='backward')
View(coefficients(backward))
#Summary of the model
Summary.Bwd <- summary(backward)
#r-squared and BIC values
Summary.Bwd$r.squared
BIC(backward)

# Computing the MSE on validation dataset based on model fit with training data
# We use the "predict" function to compute the predicted value on validation set
PredBase<-predict(backward, validation, se.fit=TRUE)  
PredBase
mean((validation[,"FARE"] - PredBase$fit)^2)


#Residual plot for final model from backward elimination
residual<-residuals(backward)
plot(residual)
hist(residual,breaks=20)
plot(backward)
