########################################################################################################
#######    Predict the sales values given data about the customer and the product info      #######    
########################################################################################################


library(mosaic) #better visualizations
library(dplyr) #Data manipulations
library(xgboost) #XGBoost 

#Read the data
train = read.csv("D:\\WorkSpace\\Black Friday Sales\\train.csv")
test = read.csv("D:\\WorkSpace\\Black Friday Sales\\test.csv")

#look at meta data 
str(train)

#Look at null values - NAN
View(colSums(is.na(train)) )

#Impute product category 2
da <- train[which(is.na(train$Product_Category_2)), ]
#Visualize for which product category 1 is the most number of values missing
bargraph(~da$Product_Category_1, data = da)
#get the value of product category 2 for which most of the values are missing
C1.5 <- filter(train,Product_Category_1 ==8)$Product_Category_2
getmode(C1.5)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#looks like the values are completely missing for Product_Category_2 against few levels of Product_Category_1
#Assign 0 to all the NAs
train[is.na(train$Product_Category_2), ]$Product_Category_2 = 0
train[is.na(train$Product_Category_3), ]$Product_Category_3 = 0

#Repeat the above step on testing
test[is.na(test$Product_Category_2), ]$Product_Category_2 = 0
test[is.na(test$Product_Category_3), ]$Product_Category_3 = 0

#Visualizations of the data
densityplot(train$Purchase, data = train)
bargraph( ~ train$Gender, data = train)
bargraph( ~ train$Age, data = train)
bargraph( ~ train$Occupation, data = train)

#Occupation should ideally be categorical. So changing it to the same.
train$Occupation = as.factor(train$Occupation)
test$Occupation = as.factor(test$Occupation)
bargraph(~ train$City_Category, data = train)
bargraph(~ train$Stay_In_Current_City_Years, data = train)
bargraph(~ train$Marital_Status, data = train)
bargraph(~ train$Product_Category_2, data = train)

#Build a regression model
#subset the train and test data exclude user ID and product ID from the analysis
traindf = subset(train, select = -c(User_ID, Product_ID))
testdf = subset(test, select = -c(User_ID, Product_ID))

####################################################
##############  Linear regression model  ########### 
####################################################
linearmodel <- lm(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3, data = traindf)
y.pred <- predict(linearmodel, testdf)

#Write the values to the submission/output file
output <- subset(test, select = c(User_ID, Product_ID))
output$Purchase = y.pred
write.csv(output, file = "D:\\WorkSpace\\Black Friday Sales\\SubmissionLM.csv")

####################################################
###################### XGBOOST #####################
####################################################

y_true <- traindf$Purchase

#One hot encoding
dummies = dummyVars(~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3, data = traindf)
df_all_ohe <- as.data.frame(predict(dummies, newdata = traindf))
#Tuning parameters
paramList <- list(eta = 0.1, 
                  gamma = 0, 
                  max.depth = 20, 
                  min_child_weight = 2, 
                  subsample = 1.0, 
                  colsample_bytree = 0.2)

#Run XGBoost model
xgb_fit = xgboost(data = as.matrix(df_all_ohe),
                  label = as.matrix(y_true), 
                  params = paramList,
                  missing = NA,
                  nrounds = 50000,
                  verbose = 1, 
                  early_stopping_rounds = 100)

#Identifying the important features from the model
importance <- xgb.importance(feature_names = colnames(df_all_ohe), model = xgb_fit)

#transform test data as One Hot Encoded matrix and make predictions for the y
df_all_test <- as.data.frame(predict(dummies, newdata = testdf))
y_pred <- predict(xgb_fit, as.matrix(df_all_test))

#Write output to submission file
output <- subset(test, select = c(User_ID, Product_ID))
output$Purchase <- y_pred
write.csv(output, file = "D:\\WorkSpace\\Black Friday Sales\\SubmissionFile.csv")

