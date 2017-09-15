setwd("/media/Madhuri/Kaggle/Titanic")
train <- read.csv("/media/Madhuri/Kaggle/Titanic/train.csv")
test <- read.csv("/media/Madhuri/Kaggle/Titanic/test.csv")

# Look at gender patterns
table(train$Sex) #summary(train$Sex)
prop.table(table(train$Survived, train$Sex))
prop.table(table(train$Sex, train$Survived), 1)

# Update Survived Column
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gendermodel.csv", row.names = FALSE)

# Look at age patterns
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
func = function(x) {
  sum(x)/length(x)
}
aggregate(Survived ~ Child + Sex, data=train, FUN=func)

# Look at class and fare patterns
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=func)

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also perish
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "genderclassmodel.csv", row.names = FALSE)