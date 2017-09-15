# setwd("F:/Kaggle/Titanic")
# train <- read.csv("F:/Kaggle/Titanic/train.csv")
# test <- read.csv("F:/Kaggle/Titanic/test.csv")

setwd("/media/pindaari/Madhuri/Kaggle/Titanic")
train <- read.csv("/media/pindaari/Madhuri/Kaggle/Titanic/train.csv")
test <- read.csv("/media/pindaari/Madhuri/Kaggle/Titanic/test.csv")


library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train$Name[1]

# Join together the test and train sets for easier feature engineering

test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)
combi$Name[1]

# Find the indexes for the tile piece of the name
strsplit(combi$Name[1], split = '[,.]')
strsplit(combi$Name[1], split = '[,.]')[[1]]
strsplit(combi$Name[1], split = '[,.]')[[1]][2]

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x){strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x){strsplit(x, split = '[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build a new tree with our new features
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "engineeredfeaturestree.csv", row.names = FALSE)

