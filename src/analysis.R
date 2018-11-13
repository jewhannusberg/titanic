# predict if a passanger would survive the titanic
# output should be to gender_submission.csv
library(tidyverse)
setwd("~/Documents/kaggle/titanic")

# read the train/test data in
# TODO: fix path by setting working directory properly
train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

##### EXPLORATORY DATA ANALYSIS #####
# view the data
head(train)
head(test)
# convert categorical 'Sex' variable to binary
unique(train$Sex)
# TODO: a better way of doing this
train[,"Sex"] <- sapply(train[,"Sex"],switch,"male"=0,"female"=1)

# how many unique values do we have in each column?
lapply(train, function(x) length(unique(x)))

# what are the unique values in columns not including PassengerId
lapply(train[-which(names(train) %in% c("PassengerId", "Name", "Ticket",
                                              "Age", "Fare", "Cabin"))], 
             function(x) unique(x))

# count NAs in each column
lapply(train, function(x) sum(is.na(x)))

# see that Age has a lot of NAs
# however age is too important a variable to just drop
# TODO: find a way to fill missing NAs
sprintf("%2f", 100.0*sum(is.na(train$Age))/nrow(train))

# create title variable that gets the title from full name
train$Title <- as.factor(gsub("(.+), ([A-Za-z]+\\.) (.+)", "\\2", train$Name, perl=TRUE))
print(unique(train$Title))

# impute the age column using the following rule
# calculate the average/median for the different titles+children combos
# insert the average/median into the NA values for age



# one hot encode Embarked column

# in model file
# try decision tree
# try xgboost
# try random forest
