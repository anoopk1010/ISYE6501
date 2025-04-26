
install.packages(kernlab)
install.packages(kknn)
install.packages(readr)
library(kernlab)
library(kknn)
library(readr)

#Question 3.1
setwd('C:\\Users\\anoop\\OneDrive\\Documents\\GA Analytics\\ISYE6501\\Homework 2\\data 3.1')
data <- read.delim('credit_card_data-headers.txt')

#Splitting data into 60% training and 40% non training
dt <- sort(sample(nrow(data),nrow(data)*.6))
train <- data[dt,]
nontrain <- data[-dt,]

#Splitting nontraining data into validation and test data (can use nontrain as test data is validation isn't needed)
dt2 <- sort(sample(nrow(nontrain),nrow(nontrain)*.5))
val <- nontrain[dt2,]
test <- nontrain[-dt2,]


acc = rep(0,29)
#Training KNN Model with different K values
for (k in 1:30)
{
  knn_model <- kknn(R1~.,train,val, k = k, scale = TRUE)
  pred <- as.integer(fitted(knn_model)+0.5)
  acc[k] = sum(pred == val$R1)/nrow(val)
  
}
acc
cat("The K Value with the Highest Accuracy: ", which.max(acc), " with an accuracy of (%): ", max(acc))

#Testing accuracy using test data from model chosen from above
knn_model <- kknn(R1~.,train, test, k=which.max(acc), scale = TRUE)
pred <- as.integer(fitted(knn_model)+0.5)
cat("The accuracy using the test data is: ", sum(pred == test$R1)/nrow(test))



# Question 4.1
# My team at work needs to determine what user-created data needs to be deleted from our databases as we are nearing storage
# capacity. As users are creating data on the daily, we are not able to easily identify which data is needed to be deleted unless
# we ask each individual user (there are 1000+ employees creating data on the daily). I could use a Clustering Model to help 
# group which data could be targeted for deletion rather than blindly asking users if their data can be removed. 
# Some factors I could consider are: Age of Creation for Data, Date of Data last accessed, Frequency of Access, and Size of the Data


#Question 4.2

