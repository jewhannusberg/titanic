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

##### Data Cleanp #####
describe_data <- function(data) {
  
  
  # how many unique values do we have in each column?
  lapply(data, function(x) length(unique(x)))
  
  # what are the unique values in columns not including PassengerId
  lapply(data[-which(names(data) %in% c("PassengerId", "Name", "Ticket",
                                          "Age", "Fare", "Cabin"))], 
         function(x) unique(x))
  
  # count NAs in each column
  lapply(data, function(x) sum(is.na(x)))
  
  # see that Age has a lot of NAs
  # however age is too important a variable to just drop
  sprintf("%2f", 100.0*sum(is.na(data$Age))/nrow(data))
}

encode_sex <- function(data) {
  # convert categorical 'Sex' variable to binary
  print(unique(data$Sex))
  data[,"Sex"] <- sapply(data[,"Sex"],switch,"male"=0,"female"=1)
  print(unique(data$Sex))
  return(data)
}

title_feature <- function(data) {
  # create title variable that gets the title from full name
  data$Title <- as.factor(gsub("(.+), ([A-Za-z]+\\.) (.+)", "\\2", data$Name, perl=TRUE))
  print(unique(data$Title))
  return(data)
}

family_feature <- function(data) {
  # combine SibSp and Parch columns to create a single family column
  data$Family <- data$SibSp + data$Parch + 1 # add 1 for the row character
  print(unique(data$Family))
  return(data)
}

drop_cabin <- function(data) {
  # how much of cabin variable is missing?
  # first replace the missing values with NA
  data$Cabin <- as.character(data$Cabin)
  
  data %<>%
    mutate(Cabin = ifelse(Cabin == "", NA, Cabin))
  
  sprintf("%2f", 100.0*sum(is.na(data$Cabin))/nrow(data)) # 77%
  # need to drop column. 77% is way too much missing values to be valuable
  data %<>%
    select(-Cabin)
  return(data)
}

# TODO: the Age variable is lost in this
impute_missing_values <- function(data) {
  # look for where values are missing
  # md.pattern(data)
  # visualise missing items a little nicer
  # aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
  
  # pick the factors we want to impute on by building a multi-linear model to see top
  # contributing factors
  age_model <- stats::lm(data=data, formula=Age ~ as.factor(Pclass) + as.factor(Sex) + SibSp + Parch +
                           as.factor(Embarked) + as.factor(Title) + as.factor(Family))
  
  age_model <- step(age_model)
  
  # use SibSp, Embarked, Family, Pclass, Title to impute Age
  
  impute_age <- data %>%
    select(Age, SibSp, Embarked, Family, Pclass, Title)
  
  imputed_age <- mice(impute_age, m=5, maxit=50, method='rf',seed=500)
  
  data_imputed <- complete(imputed_age, 1)
  
  # view the distribution of the imputed age against the original distribution
  p1 <- data %>% # original distribution
    ggplot(aes(Age)) +
    geom_histogram(position = "dodge") + # geom_hist manually removes NA values
    labs(x = "Age", y = "Count", title = "Distribution of original age values")

  p2 <- data_imputed %>% # new distribution
    ggplot(aes(Age)) +
    geom_histogram(fill="red", position = "dodge") + # geom_hist manually removes NA values
    labs(x = "Age", y = "Count", title = "Distribution of imputed age values")

  gridExtra::grid.arrange(p1, p2, nrow=2) # distribution looks pretty good

  data$Age <- data_imputed$Age
  
  return(data)
}

setwd("~/Documents/kaggle/titanic")

# read the train/test data in
# TODO: fix path by setting working directory properly
train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

# run the training and test sets seperately through the cleanup code
train <- drop_cabin(train)
train <- encode_sex(train)
train <- family_feature(train)
train <- title_feature(train)
train <- impute_missing_values(train)

test <- drop_cabin(test)
test <- drop_cabin(test)
test <- encode_sex(test)
test <- family_feature(test)
test <- title_feature(test)
test <- impute_missing_values(test)


# what does a decision tree say about the data simply as is?
# fit.salary <- rpart(Survived ~ , train)
# rpart.plot::prp(fit.salary)
