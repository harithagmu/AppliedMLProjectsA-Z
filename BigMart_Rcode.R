library(dplyr)
library(ggplot2)
library(mosaic)
library(mice)

setwd("D:\\WorkSpace\\BigMartSales")
train_original = read.csv("train.txt")
test_original = read.csv("test.txt")
data = bind_rows(train_original, test_original)

#Checking the missing data
View(colSums(is.na(data)))
#Summary stats for each variable
str(data)
summary(data)

#Looking at the pattern of missing data of weight column
missingdata = filter(data, is.na(Item_Weight)==TRUE)
barplot(table(missingdata$Item_Type), data = missingdata) # Data is missing at random

#Using imputers to replace the null values of weight column
data[is.na(data)] <- 0

#MODEL
output <- data[8524:14204, c("Item_Identifier", "Outlet_Identifier")]
train <- data[1:8523, c("Item_Weight","Item_Fat_Content","Item_Visibility","Item_Type","Item_MRP","Outlet_Identifier","Outlet_Establishment_Year","Outlet_Size","Outlet_Location_Type","Outlet_Type","Item_Outlet_Sales")]
test <- data[8524:14204, c("Item_Weight","Item_Fat_Content","Item_Visibility","Item_Type","Item_MRP","Outlet_Identifier","Outlet_Establishment_Year","Outlet_Size","Outlet_Location_Type","Outlet_Type")]

#Using random forest
library(randomForest) 
rfclassifier = randomForest(Item_Outlet_Sales ~ ., data = train)
target = predict(rfclassifier, newdata = test)
output$Item_Outlet_Sales <- target
write.csv(output, "Solution.csv")
