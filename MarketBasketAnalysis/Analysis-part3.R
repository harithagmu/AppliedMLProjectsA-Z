#Part 3
#Loading required packages
library(arules)
setwd("D:\\SPRING 2019\\OR 568\\HomeWorks\\HW04")
mydata<-read.csv("Groceries.csv",header=TRUE)
mydata[1:5,]
View(mydata)
nrow(mydata)
#Generate a bar-chart plot for the top 5 items with highest frequency and 
#report the corresponding items?
mydata$ID = factor(mydata$ID)
groceryData <- split(x=mydata[,"Product"],f=mydata$ID)

View(groceryData)
#Removing duplicates in each transaction
groceryData <- lapply(groceryData,unique) 
groceryData[1:2]

#Viewing this as a list of transactions
groceryData <- as(groceryData,"transactions") 
itemFrequency(groceryData)

#Generate a set of association rules by setting the minimum support threshold value at 0.005,
#and the minimum confidence threshold value at 0.2
#using apriori
groceryrules <- apriori(groceryData, parameter=list(support=.005,confidence=.2)) 
#Report the total number of associations rules obtained and the top 5 rules with highest lift values.
inspect(groceryrules)
gh <- inspect(head(sort(groceryrules, by = "lift"), 5))
View(gh)
#soda rules
soda_rules = apriori(groceryData, parameter = list(support = 0.005,confidence = 0.2, maxlen = 2), appearance = list(default="lhs",rhs=" soda"))
sr <- inspect(sort(soda_rules, by = "lift"))
soda_rules1 = apriori(groceryData, parameter = list(support = 0.005,confidence = 0.2, maxlen = 2), appearance = list(default="rhs",lhs=" soda"))
sr <- inspect(sort(soda_rules1, by = "lift"))
#Cross-sell butter
butter_rules = apriori(groceryData, parameter = list(support = 0.005,confidence = 0.2), appearance = list(default="lhs",rhs=" butter"))
inspect(sort(butter_rules, by = "lift"))
butter_rules1 = apriori(groceryData, parameter = list(support = 0.005,confidence = 0.2, maxlen = 2), appearance = list(default="rhs",lhs=" butter"))
inspect(sort(butter_rules1, by = "lift"))






