# predict if a passanger would survive the titanic
# output should be to gender_submission.csv
library(tidyverse)
library(magrittr)
library(rpart)
library(mice)
library(VIM) # for better visualisations of missing data
library(DMwR) # trying out combinations of LMs


# TODO
# DONE -- 1. finish cleaning the data
# 2. modulate the cleaning component to run train and test seperately
# 3. EDA
# 4. model the data using a. decision tree, b. random forest, c. xgboost

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

# combine SibSp and Parch columns to create a single family column
train$Family <- train$SibSp + train$Parch + 1 # add 1 for the row character


# TODO: impute the age column using the following rule
# calculate the average/median for the different titles+children combos
# insert the average/median into the NA values for age
# sibsp and parch are columns that should be useful in imputing age
# start by observing their distributions

train %>%
  select(Age, Survived) %>%
  ggplot(aes(Age)) + 
  geom_histogram(position = "dodge") + # geom_hist manually removes NA values
  labs(x = "Age", y = "Count", title = "Count of Age values")

train %>%
  select(SibSp, Survived) %>%
  ggplot(aes(SibSp)) + 
  geom_histogram(position="dodge") +
  labs(x = "SibSp", y = "Count", title = "Count of Siblings/Spouse values")

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

# pick the factors we want to impute on by building a multi-linear model to see top
# contributing factors
age_model <- stats::lm(data=train, formula=Age ~ as.factor(Pclass) + as.factor(Sex) + SibSp + Parch +
                         as.factor(Embarked) + as.factor(Title) + as.factor(Family))

age_model <- step(age_model)

# use SibSp, Embarked, Family, Pclass, Title to impute Age

impute_age <- train %>%
  select(Age, SibSp, Embarked, Family, Pclass, Title)

imputed_age <- mice(impute_age, m=5, maxit=50, method='rf',seed=500)

# view the distribution of the imputed age against the original distribution
p1<-train %>% # original distribution
  ggplot(aes(Age)) + 
  geom_histogram(position = "dodge") + # geom_hist manually removes NA values
  labs(x = "Age", y = "Count", title = "Distribution of original age values")

p2<-impute_age %>% # original distribution
  ggplot(aes(Age)) + 
  geom_histogram(fill="red", position = "dodge") + # geom_hist manually removes NA values
  labs(x = "Age", y = "Count", title = "Distribution of imputed age values")

gridExtra::grid.arrange(p1,p2,nrow=2) # distribution looks pretty good

# insert the imputed age into the original data set
train$Age <- imputed_age$Age
sum(is.na(train$Age)) # check if still any missing values?

# how much of cabin variable is missing?
# first replace the missing values with NA
train$Cabin <- as.character(train$Cabin)

train %<>%
  mutate(Cabin = ifelse(Cabin == "", NA, Cabin))

sprintf("%2f", 100.0*sum(is.na(train$Cabin))/nrow(train)) # 77%
# need to drop column. 77% is way too much missing values to be valuable
train %<>%
  select(-Cabin)

# start some EDA!

# what does a decision tree say about the data simply as is?
# fit.salary <- rpart(Survived ~ , train)
# rpart.plot::prp(fit.salary)
