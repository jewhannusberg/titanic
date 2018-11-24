# model the data
library(xgboost)

##### XGBOOST #####
# keep data that matters
data2 <- data[, -c(1,3,4,9, 15,17)]


data2$Title[data2$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
data2$Title[data2$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
data2$Title[data2$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
data2$Title <- factor(data2$Title)


# xgboost requires start from 0
data2$Pclass <- as.numeric(data2$Pclass)- 1
data2$Embarked <- as.numeric(data2$Embarked) - 1
data2$Title <- as.numeric(data2$Title) - 1
data2$Family <- as.numeric(data2$Family) - 1

str(data2)
data2 <- data2[, -c(5)]
# as matrix for xgboost
data2 <- as.matrix(data2)

# Splitting back to train and test sets
test <- data2[1:418,]
train <- data2[419:1309,]

param <- list("objective" = "binary:logistic")

xgboost_cv = xgb.cv(param = param, data = train[, -c(6)],
                    label = train[, c(6)], nfold = 3, nrounds = 15)

# Fitting with the xgboost model
fit_xgboost <- xgboost(param = param, data = train[, -c(6)],
                       label = train[, c(6)], nfold = 3, nrounds = 15)

# Get the feature real names
names <- dimnames(train)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = fit_xgboost)

# Plotting
xgb.plot.importance(importance_matrix)

# Prediction on test and train sets
pred_xgboost_test <- predict(fit_xgboost, test[, -c(6)])
pred_xgboost_train <- predict(fit_xgboost, train[, -c(6)])

# Since xgboost gives a survival probability prediction, we need to find the best cut-off:
proportion <- sapply(seq(.3,.7,.01),function(step) c(step,sum(ifelse(pred_xgboost_train<step,0,1)!=train[, c(6)])))
dim(proportion)
# Applying the best cut-off on the train set prediction for score checking
predict_xgboost_train <- ifelse(pred_xgboost_train<proportion[,which.min(proportion[2,])][1],0,1)
head(predict_xgboost_train)
score <- sum(train[, c(6)] == predict_xgboost_train)/nrow(train)
score

# Applying the best cut-off on the test set
predict_xgboost_test <- ifelse(pred_xgboost_test<proportion[,which.min(proportion[2,])][1],0,1)
test <- as.data.frame(test) # Conveting the matrix into a dataframe

# Creating the submitting file
submit <- data.frame(PassengerId = data[892:1309,c("PassengerId")], Survived = predict_xgboost_test)
write.csv(submit, file = "data/titanic_xgboost.csv", row.names = FALSE)

#########################################################################


