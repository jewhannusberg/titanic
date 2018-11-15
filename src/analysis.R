# predict if a passanger would survive the titanic
# output should be to gender_submission.csv
library(tidyverse)
library(magrittr)
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
sprintf("%2f", 100.0*sum(is.na(train$Age))/nrow(train))

# create title variable that gets the title from full name
train$Title <- as.factor(gsub("(.+), ([A-Za-z]+\\.) (.+)", "\\2", train$Name, perl=TRUE))
print(unique(train$Title))

# TODO: impute the age column using the following rule
# calculate the average/median for the different titles+children combos
# insert the average/median into the NA values for age
# sibsp and parch are columns that should be useful in imputing age
# start by observing their distributions

train %>%
  select(Age, Survived) %>%
  ggplot(aes(Age)) + 
  geom_histogram(position = "dodge") + # geom_hist manually removes NA values
  labs(x = "Parch", y = "Count", title = "Count of Age values")

train %>%
  select(SibSp, Survived) %>%
  ggplot(aes(SibSp)) + 
  geom_histogram(position="dodge") +
  labs(x = "Parch", y = "Count", title = "Count of Siblings/Spouse values")

train %>%
  select(Parch, Survived) %>%
  ggplot(aes(Parch)) + 
  geom_histogram(position="dodge") +
  labs(x = "Parch", y = "Count", title = "Count of Parent/Child values")

# how much of cabin variable is missing?
# first replace the missing values with NA
train$Cabin <- as.character(train$Cabin)

train %<>%
  mutate(Cabin = ifelse(Cabin == "", NA, Cabin))

sprintf("%2f", 100.0*sum(is.na(train$Cabin))/nrow(train)) # 77%
# need to drop column. 77% is way too much missing values to be valuable
train %<>%
  select(-Cabin)


# one hot encode Embarked column

# in model file
# try decision tree
# try xgboost
# try random forest
