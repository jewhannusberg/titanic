# model the data
library(xgboost)

##### XGBOOST #####
# keep features that contribute to the outcome
model_data <- data %>%
  select(Family, Age, Pclass, SibSp, Title, Embarked, Parch, Survived)

model_data$Title <- factor(model_data$Title)

# xgboost requires start from 0
model_data$Pclass <- as.numeric(model_data$Pclass) - 1
model_data$Embarked <- as.numeric(model_data$Embarked) - 1
model_data$Title <- as.numeric(model_data$Title) - 1
model_data$Family <- as.numeric(model_data$Family) - 1

# as matrix for xgboost
model_data <- as.matrix(model_data)

# go back to train and test splits
test <- model_data[1:418,]
train <- model_data[419:1309,]

param <- list("objective" = "binary:logistic")

# cross validation of the xgboost
xgboost_cv = xgb.cv(param = param, data = train[, -c(8)],
                    label = train[, c(8)], nfold = 3, nrounds = 15)

# fit the xgboost to the training data
fit_xgboost <- xgboost(param = param, data = train[, -c(8)],
                       label = train[, c(8)], nfold = 3, nrounds = 15)

# names of the features
names <- dimnames(train)[[2]]

# feature importance matrix
importance_matrix <- xgb.importance(names, model = fit_xgboost)
# I feel as though we're actually overfitting!
xgb.plot.importance(importance_matrix)

# prediction on train and test
pred_xgboost_test <- predict(fit_xgboost, test[, -c(8)])
pred_xgboost_train <- predict(fit_xgboost, train[, -c(8)])

# since xgboost gives a survival probability prediction, we need to find the best cut-off:
proportion <- sapply(seq(.3, .7, .01), 
                     function(step) c(step, sum(ifelse(pred_xgboost_train < step, 0, 1) !=
                                                 train[, c(8)])))
dim(proportion)

# apply the cut-off on the train set prediction for score checking
predict_xgboost_train <- ifelse(pred_xgboost_train <
                                  proportion[, which.min(proportion[2, ])][1], 0, 1)
head(predict_xgboost_train)
score <- sum(train[, c(8)] == predict_xgboost_train) / nrow(train)
score

# apply the best cut-off on the test set
predict_xgboost_test <- ifelse(pred_xgboost_test <
                                 proportion[, which.min(proportion[2, ])][1],0,1)
test <- as.data.frame(test) # Conveting the matrix into a dataframe

# submit the results
submit <- data.frame(PassengerId = data[892:1309,c("PassengerId")],
                     Survived = predict_xgboost_test)
write.csv(submit, file = "data/titanic_xgboost.csv", row.names = FALSE)

#########################################################################


