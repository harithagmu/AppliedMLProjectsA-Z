#Improved visualizations
require(mosaic)
#f1 score and accuracy metrics
require(caret)
#Summary statistics 
summary(iris)
#Summary stats for each variable by the target level
cnt <- c(1:4)
for (i in cnt)
{
  print(paste("*********", colnames(iris[i]), "**********"))
  print(favstats(iris[i]~iris$Species))
}

#Looking at Meta data
str(iris)

#Visualizing sepal length by species
plot(iris$Species, iris$Sepal.Length, data=iris, main ="sepal length by species")
#Visualizing sepal width by species
plot(iris$Species, iris$Sepal.Width, data=iris, main ="sepal width by species")
#Visualizing petal length by species
plot(iris$Species, iris$Petal.Length, data=iris, main ="petal length by species")
#Visualizing petal width by species
plot(iris$Species, iris$Petal.Width, data=iris, main ="petal width by species")

#Checking if data is normalized
densityplot(iris$Sepal.Length)
densityplot(iris$Sepal.Width)
densityplot(iris$Petal.Length) # bi-modal distribution. Need for normalization
densityplot(iris$Petal.Width) # bi-modal distribution. Need for normalization

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))

#Checking for null values
colSums(is.na(iris)) 

#Dividing the data into train and test data sets
row <- nrow(iris)
set.seed(100)
trainindex <- sample(row, row*.7, replace=FALSE)
training <- iris_norm[trainindex, ]
validation <- iris_norm[-trainindex, ]
traininglabel <- iris[trainindex,5]
testinglabel <- iris[-trainindex,5]

#Applying machine learning models
##load the package class
library(class)
require(class)
##run knn function
Accuracylist = c()
cnt <- c(1:10)
for (i in cnt)
{
  knniris <- knn(training,validation,cl=traininglabel, k = i, use.all = TRUE)
  ##create confusion matrix
  cm <- table(knniris, testinglabel)
  accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
  Accuracylist[[i]] <- accuracy(cm)
  op = paste("for neighbor count = ", i ," accuracy = ", Accuracylist[[i]])
  print(op)
}

#Find the k value of highest accuracy
print(paste("k = ", which.max(Accuracylist), " has highest accuracy of ", max(Accuracylist)))

#F1-Score
f1_score(knniris, testinglabel)


#Multinomila model
library(nnet)
training <- iris[trainindex, ]
validation <- iris[-trainindex, ]
validation[,5] <- NULL
multinomModel <- multinom(Species~., data = training)
predicted <- predict(multinomModel, validation)
#Confusion matrix
cm <- table(predicted, testinglabel)
#accuracy score
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(cm)
#F1-Score
f1_score(predicted, testinglabel)
