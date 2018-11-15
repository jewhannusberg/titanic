# predict if a passanger would survive the titanic
# output should be to gender_submission.csv
library(tidyverse)
library(magrittr)
library(rpart)
library(mice)
library(VIM) # for better visualisations of missing data

setwd("~/Documents/kaggle/titanic")

# read the train/test data in
# TODO: fix path by setting working directory properly
train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

##### EXPLORATORY DATA ANALYSIS #####
# view the data
head(train)
head(test)

summary(train) 
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

# look for where values are missing
md.pattern(train)
# visualise missing items a little nicer
aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# impute the missing data with mice package
# TODO: study what the hell this package is doing!
# method(mice) to see all the different methods
# THIS DOESN'T WORK -- TOO MANY FACTORS GOING INTO MICE?
imputed_train <- mice(train, m=5, maxit=50, method='pmm',seed=500) # predictive mean matching
imputed_train$imp$Age # check the imputed data




# how much of cabin variable is missing?
# first replace the missing values with NA
train$Cabin <- as.character(train$Cabin)

train %<>%
  mutate(Cabin = ifelse(Cabin == "", NA, Cabin))

sprintf("%2f", 100.0*sum(is.na(train$Cabin))/nrow(train)) # 77%
# need to drop column. 77% is way too much missing values to be valuable
train %<>%
  select(-Cabin)

# what does a decision tree say about the data simply as is?
fit.salary <- rpart(Survived ~ , train)
rpart.plot::prp(fit.salary)
