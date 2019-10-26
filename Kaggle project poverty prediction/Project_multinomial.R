#Loading libraries
#Install.packages(<library name>) to install any package before using it
library(randomForest)
library(dplyr)
library(tidyverse)
library(xgboost)
library(glmnet)
library(caret)
library(nnet)

setwd("d:\...\...")
povertyData<-read.csv('train.csv')
povertyDataTest<-read.csv('test.csv')
samp<-povertyDataTest$Id
###################################################################################################

###########################################################################################
#Train
colnames(povertyData)[colSums(is.na(povertyData)) > 0]
sapply(povertyData, function(x) sum(is.na(x)))[colSums(is.na(povertyData)) > 0]
nullValueCols<-sapply(training, function(x) sum(is.na(x)))

povertyDataV2Na <- povertyData[rowSums(is.na(povertyData['v2a1'])) > 0,]

houseOwnership=c(table(povertyDataV2Na$tipovivi1)[2],table(povertyDataV2Na$tipovivi2)[2],table(povertyDataV2Na$tipovivi3)[2],table(povertyDataV2Na$tipovivi4)[2])

barplot(houseOwnership,names.arg=c("Owned and paid","Owned-paying","Rented","Precarious"),col=c("light green","red","green","yellow"),las=0.5)


######Number of tablets#######################################
povertyDataV18Na <- povertyData[rowSums(is.na(povertyData['v18q1'])) > 0,]

tabletOwnership=c(table(povertyDataV18Na$v18q))

######Years behind in school#######################################

povertyDataVrez <- povertyData[rowSums(is.na(povertyData['rez_esc'])) == 0,]
summary(povertyDataVrez$age)
############################Convet all NAs to zeors##################################
povertyData[is.na(povertyData)] <- 0
#train
#Check if all nulls are trated
colnames(povertyData)[colSums(is.na(povertyData)) > 0]

#test
povertyDataTest[is.na(povertyDataTest)] <- 0
############################Feature Engineering###################################
#Removing all columns which are squared
#Train
povertyData <- povertyData[,-(134:140),drop=FALSE]

#Test
povertyDataTest <- povertyDataTest[,-(134:140),drop=FALSE]
#povertyData <- povertyData[ , -which(names(povertyData) %in% c("female"))]



has_many_values <- function(x) n_distinct(x) > 1
dup_var <- function(x) lapply(x, c) %>% duplicated %>% which 

#Aggregate individual variable to household level
povertyData <- povertyData %>%
  group_by(idhogar) %>%
  mutate(mean_age = mean(age, na.rm = TRUE)) %>%
  mutate(no_of_disabled = sum(dis)) %>%
  mutate(no_of_children = sum(estadocivil1)) %>%
  mutate(no_of_coupledunion = sum(estadocivil2)) %>%
  mutate(no_of_married = sum(estadocivil3)) %>%
  mutate(no_of_divorced = sum(estadocivil4)) %>%
  mutate(no_of_separated = sum(estadocivil5)) %>%
  mutate(no_of_widower = sum(estadocivil6)) %>%
  mutate(no_of_single = sum(estadocivil7)) %>%
  mutate(no_of_instlevel1 = sum(instlevel1)) %>%
  mutate(no_of_instlevel2 = sum(instlevel2)) %>%
  mutate(no_of_instlevel3 = sum(instlevel3)) %>%
  mutate(no_of_instlevel4 = sum(instlevel4)) %>%
  mutate(no_of_instlevel5 = sum(instlevel5)) %>%
  mutate(no_of_instlevel6 = sum(instlevel6)) %>%
  mutate(no_of_instlevel7 = sum(instlevel7)) %>%
  mutate(no_of_instlevel8 = sum(instlevel8)) %>%
  mutate(no_of_instlevel9 = sum(instlevel9)) %>%
  mutate(rent_rooms = (v2a1 / rooms)) %>%
  mutate(tamhog_rooms = (tamhog / rooms)) %>%
  mutate(v2a1_r4t1_r4t3 = (v2a1 / (r4t3 - r4t1))) %>%
  mutate(v2a1_r4t1 = (v2a1 / r4t1)) %>%
  mutate(v2a1_r4t2 = (v2a1 / r4t2)) %>%
  mutate(v2a1_r4t3 = (v2a1 / r4t3)) %>%
  mutate(v2a1_r4h1 = (v2a1 / r4h1)) %>%
  mutate(v2a1_r4h2 = (v2a1 / r4h2)) %>%
  mutate(v2a1_r4h3 = (v2a1 / r4h3)) %>%
  mutate(v2a1_r4m1 = (v2a1 / r4m1)) %>%
  mutate(v2a1_r4m2 = (v2a1 / r4m2)) %>%
  mutate(v2a1_r4m3 = (v2a1 / r4m3)) %>%
  mutate(v2a1_bedrooms = (v2a1 / bedrooms)) %>%
  mutate(r4t1_r4t3 = (r4t1 / r4t3)) %>%
  mutate(v2a1_tamhog = (v2a1 / tamhog)) %>%
  mutate(r4m1_r4t3 = (r4m1 / r4t3)) %>%
  mutate(rooms_r4t1 = (rooms / r4t1)) %>%
  mutate(v18q1_r4t3 = (v18q1 / r4t3)) %>%
  mutate(r4h3_r4t3 = (r4h3 / r4t3)) %>%
  mutate(r4m3_r4t3 = (r4m3 / r4t3)) %>%
  mutate(v18q1_r4t3 = (v18q1 / r4t2)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  mutate(r4t3_tamhog = (r4t3 / tamhog)) %>%
  mutate(r4t3_rooms = (r4t3 / rooms)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  ungroup()


#Aggregate individual variable to household level
povertyDataTest <- povertyDataTest %>%
  group_by(idhogar) %>%
  mutate(mean_age = mean(age, na.rm = TRUE)) %>%
  mutate(no_of_disabled = sum(dis)) %>%
  mutate(no_of_children = sum(estadocivil1)) %>%
  mutate(no_of_coupledunion = sum(estadocivil2)) %>%
  mutate(no_of_married = sum(estadocivil3)) %>%
  mutate(no_of_divorced = sum(estadocivil4)) %>%
  mutate(no_of_separated = sum(estadocivil5)) %>%
  mutate(no_of_widower = sum(estadocivil6)) %>%
  mutate(no_of_single = sum(estadocivil7)) %>%
  mutate(no_of_instlevel1 = sum(instlevel1)) %>%
  mutate(no_of_instlevel2 = sum(instlevel2)) %>%
  mutate(no_of_instlevel3 = sum(instlevel3)) %>%
  mutate(no_of_instlevel4 = sum(instlevel4)) %>%
  mutate(no_of_instlevel5 = sum(instlevel5)) %>%
  mutate(no_of_instlevel6 = sum(instlevel6)) %>%
  mutate(no_of_instlevel7 = sum(instlevel7)) %>%
  mutate(no_of_instlevel8 = sum(instlevel8)) %>%
  mutate(no_of_instlevel9 = sum(instlevel9)) %>%
  mutate(rent_rooms = (v2a1 / rooms)) %>%
  mutate(tamhog_rooms = (tamhog / rooms)) %>%
  mutate(v2a1_r4t1_r4t3 = (v2a1 / (r4t3 - r4t1))) %>%
  mutate(v2a1_r4t1 = (v2a1 / r4t1)) %>%
  mutate(v2a1_r4t2 = (v2a1 / r4t2)) %>%
  mutate(v2a1_r4t3 = (v2a1 / r4t3)) %>%
  mutate(v2a1_r4h1 = (v2a1 / r4h1)) %>%
  mutate(v2a1_r4h2 = (v2a1 / r4h2)) %>%
  mutate(v2a1_r4h3 = (v2a1 / r4h3)) %>%
  mutate(v2a1_r4m1 = (v2a1 / r4m1)) %>%
  mutate(v2a1_r4m2 = (v2a1 / r4m2)) %>%
  mutate(v2a1_r4m3 = (v2a1 / r4m3)) %>%
  mutate(v2a1_bedrooms = (v2a1 / bedrooms)) %>%
  mutate(r4t1_r4t3 = (r4t1 / r4t3)) %>%
  mutate(v2a1_tamhog = (v2a1 / tamhog)) %>%
  mutate(r4m1_r4t3 = (r4m1 / r4t3)) %>%
  mutate(rooms_r4t1 = (rooms / r4t1)) %>%
  mutate(v18q1_r4t3 = (v18q1 / r4t3)) %>%
  mutate(r4h3_r4t3 = (r4h3 / r4t3)) %>%
  mutate(r4m3_r4t3 = (r4m3 / r4t3)) %>%
  mutate(v18q1_r4t3 = (v18q1 / r4t2)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  mutate(r4t3_tamhog = (r4t3 / tamhog)) %>%
  mutate(r4t3_rooms = (r4t3 / rooms)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  mutate(r4h3_r4m3 = (r4h3 / r4m3)) %>%
  ungroup()




povertyData <- povertyData %>%
  select(-tamviv) %>% # number of persons living in the household
  select(-hogar_total) %>% # # of total individuals in the household
  select(-r4t3) %>% # Total persons in the household
  select(-tamhog) %>% # size of the household
  select(-r4t1) %>% # persons younger than 12 years of age
  select(-r4t2) %>% # persons 12 years of age and older
  select(-agesq) %>% # Age squared
  select(-Id) %>% # removing id
  select(-idhogar)


povertyDataTest <- povertyDataTest %>%
  #select(-tamviv) %>% # number of persons living in the household
  #select(-hogar_total) %>% # # of total individuals in the household
  #select(-r4t3) %>% # Total persons in the household
  #select(-tamhog) %>% # size of the household
  #select(-r4t1) %>% # persons younger than 12 years of age
  #select(-r4t2) %>% # persons 12 years of age and older
  #select(-agesq) %>% # Age squared
  select(-Id) %>% # removing id
  select(-idhogar)

# Check if the dataset has any non numeric column(s)
povertyData %>%
  select_if(funs(!is.numeric(.)))

povertyDataTest %>%
  select_if(funs(!is.numeric(.)))

# Recode values in dependency, edjefe, edjefa
povertyData[,c("dependency","edjefe","edjefa")] <- povertyData %>% 
  select(dependency,edjefe,edjefa) %>% 
  mutate_all(funs(ifelse(. == "yes",1,ifelse(. == "no",0,.)))) %>% 
  mutate_all(as.numeric)

povertyDataTest[,c("dependency","edjefe","edjefa")] <- povertyDataTest %>% 
  select(dependency,edjefe,edjefa) %>% 
  mutate_all(funs(ifelse(. == "yes",1,ifelse(. == "no",0,.)))) %>% 
  mutate_all(as.numeric)


row<-nrow(povertyData)
set.seed(12345)
trainindex <- sample(row, row*.7, replace=FALSE)
training <- povertyData[trainindex, ]
validation <- povertyData[-trainindex, ]

#Train-test data 
train_labels <- as.numeric(training$Target) - 1
test_labels<-as.numeric(validation$Target)-1
train_data <- as.matrix(training[,-127])
test_data <- as.matrix(validation[,-127])

trainingSubset <- subset( training, select = -elimbasu5 )
validationSubset <- subset( validation, select = -elimbasu5 )

#Imputing the target variable
trainingSubset$Target[trainingSubset$Target == 1] = 'Extreme Poverty'
trainingSubset$Target[trainingSubset$Target == 2] = 'Moderate Poverty'
trainingSubset$Target[trainingSubset$Target == 3] = 'Vulnerable Household'
trainingSubset$Target[trainingSubset$Target == 4] = 'Non-Vulnerable Household'
validationSubset$Target[validationSubset$Target == 1] = 'Extreme Poverty'
validationSubset$Target[validationSubset$Target == 2] = 'Moderate Poverty'
validationSubset$Target[validationSubset$Target == 3] = 'Vulnerable Household'
validationSubset$Target[validationSubset$Target == 4] = 'Non-Vulnerable Household'

#Training the model
multinomModel <- multinom(Target ~ ., data=trainingSubset)
#predicting the target with the above model
Multinompred <- predict(multinomModel, validationSubset)
validation_labels<-validationSubset$Target

#confusion matric and f-score calculation
cm <- table(Multinompred, validation_labels)
f1_score(Multinompred, validation_labels)

#Wald-test for significance of predictors
z <- summary(multinomModel)$coefficients/summary(multinomModel)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

#Second iteration -> edjefa and agesq were identified as less significant for the model using wald-test
trainingSubset <- subset( trainingSubset, select = -edjefa )
validationSubset <- subset( validationSubset, select = -edjefa )
trainingSubset <- subset( trainingSubset, select = -agesq )
validationSubset <- subset( validationSubset, select = -agesq )

#Training the model
multinomModel2 <- multinom(Target ~ ., data=trainingSubset)
#Predicting class label using 2nd model
Multinompred2_2 <- predict(multinomModel2, validationSubset)
#Confusion matrix and f1-score
cm <- table(Multinompred2, validation_labels)
f1_score(Multinompred2_2, validation_labels)

#Model comparision between the full model and partial model
anova(multinomModel, multinomModel2)