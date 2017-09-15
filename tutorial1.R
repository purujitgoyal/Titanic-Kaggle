# Set working directory and import datafiles

setwd("/media/Madhuri/Kaggle/Titanic")
train <- read.csv("/media/Madhuri/Kaggle/Titanic/train.csv")
# train <- read.csv("train.csv", stringsAsFactors=FALSE)
  View(train)
test <- read.csv("/media/Madhuri/Kaggle/Titanic/test.csv")
  View(test)

# Structure of dataframe train    
str(train)

table(train$Survived)
prop.table(table(train$Survived))

# Add Survived column to test datafile and set 0 to all
test$Survived <- rep(0, 418)

# Write output
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)