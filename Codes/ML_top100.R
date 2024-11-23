library(MASS)
library(caTools)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(qdapRegex)
library(gbm)
library(boot)
library(dplyr)
library(ggplot2)
library(gplots)
library(GGally)
library(car)
library(ROCR)
library(pROC)
library(mlbench)
library(caretEnsemble)
library(glmnet)


songs = read.csv("data/songs_complete_data.csv", stringsAsFactors=FALSE)
songs = songs %>% filter(Release_Year >= 2000, Release_Year < 2019)
# head(songs)
songs$X = NULL
songs$Artist = NULL
songs$Title = NULL
songs$URI = NULL
songs$Release_Year = NULL
top100 = songs$Top100
songs$lyrics = NULL
songs$Top100 = NULL

songs$Danceability = scale(songs$Danceability)
songs$Energy = scale(songs$Energy)
songs$Loudness = scale(songs$Loudness)
songs$Speechiness = scale(songs$Speechiness)
songs$Acousticness = scale(songs$Acousticness)
songs$Instrumentalness = scale(songs$Instrumentalness)
songs$Liveness = scale(songs$Liveness)
songs$Valence = scale(songs$Valence)
songs$Tempo = scale(songs$Tempo)
songs$Duration = scale(songs$Duration)

songs$Key = as.factor(songs$Key)
songs$Mode = as.factor(songs$Mode)
songs$Time_Signature = as.factor(songs$Time_Signature)
songs$explicit = as.factor(songs$explicit)
songs$Genre = as.factor(songs$Genre)

dmy <- dummyVars(" ~ .", data=songs, fullRank = TRUE)
songs <- data.frame(predict(dmy, newdata=songs))

songs$Top100 = as.factor(top100)


set.seed(123)

# Building ML Models
# ```{r}
# helper functions to calculate accuracy, TPR, and FPR

tableAccuracy <- function(test, pred) {
  t = table(test, pred)
  a = sum(diag(t))/length(test)
  return(a)
}

tableTPR <- function(label, pred) {
  t = table(label, pred)
  return(t[2,2]/(t[2,1] + t[2,2]))
}

tableFPR <- function(label, pred) {
  t = table(label, pred)
  return(t[1,2]/(t[1,1] + t[1,2]))
}

# Train-Test Split

spl = sample.split(songs$Top100, SplitRatio = 0.75)
train = songs %>% filter(spl == TRUE)
test = songs %>% filter(spl == FALSE)

levels(train$Top100) <- make.names(levels(factor(train$Top100)))
levels(test$Top100) <- make.names(levels(factor(test$Top100)))

# make model matrices
train.mm = as.data.frame(model.matrix(Top100 ~ . + 0, data=train))
test.mm = as.data.frame(model.matrix(Top100 ~ . + 0, data=test)) 


# Model 0: Baseline Model

table(train$Top100) # most frequent is "Not Top 100"
table(test$Top100) # baseline: predict always "Not Top 100"
baseline_accuracy = 1414/(1414+357) # 0.7984
baseline_accuracy


# Model 1: Logistic Regression

set.seed(123)

log_model = glm(Top100 ~ .-Time_Signature.4-Genre.rap, data = train, family = "binomial")
vif(log_model) # Removed Time_Signature.4 and Genre.rap because of high VIF score
summary(log_model)

predict_log = predict(log_model, newdata = test, type = "response")
table(test$Top100, predict_log > 0.5)
tableAccuracy(test$Top100, predict_log > 0.5) # 0.8057595
tableTPR(test$Top100, predict_log > 0.5) # 0.1372549
tableFPR(test$Top100, predict_log > 0.5) # 0.02545969

# Calculate ROC curve
rocr.log.pred <- prediction(predict_log, test$Top100)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)

as.numeric(performance(rocr.log.pred, "auc")@y.values) # AUC = 0.7422088




# Model 2: Improved Logistic Regression

set.seed(123)

#Selecting only significiant features (excluding lyrics)
log_model = glm(Top100 ~ .-Danceability-Key.1-Key.2-Key.3-Key.4-Key.5-Key.6-Key.7-Key.9-Key.10-Key.11-
                  Mode.1-Speechiness-Time_Signature.3-Time_Signature.4-Time_Signature.5-Genre.classical-
                  Genre.edm-Genre.jazz-Genre.rap-Genre.metal-Genre.reggae-Genre.rock, 
                data = train, family = "binomial")
vif(log_model)
summary(log_model)

predict_log = predict(log_model, newdata = test, type = "response")
table(test$Top100, predict_log > 0.5)
tableAccuracy(test$Top100, predict_log > 0.5) # 0.8068888
tableTPR(test$Top100, predict_log > 0.5) # 0.140056
tableFPR(test$Top100, predict_log > 0.5) # 0.02475248

# Calculate ROC curve
rocr.log.pred <- prediction(predict_log, test$Top100)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)

as.numeric(performance(rocr.log.pred, "auc")@y.values) # AUC = 0.7425772


# Model 9: Ridge Regression

set.seed(123)

# convert training data to matrix format
x <- model.matrix(Top100 ~ ., train)
# convert class to numerical variable
y <- ifelse(train$Top100=='X1',1,0)

# perform grid search to find optimal value of lambda
# family = binomial => logistic regression, alpha = 0 => ridge
# check docs to explore other type.measure options
cv.out <- cv.glmnet(x, y, alpha = 0, family = 'binomial', type.measure = 'mse')
#plot result
plot(cv.out)

# min value of lambda
lambda_min <- cv.out$lambda.min
# best value of lambda
lambda_1se <- cv.out$lambda.1se
# regression coefficients
coef(cv.out,s=lambda_1se)

# get test data
x_test <- model.matrix(Top100 ~ ., test)
# predict class, type=”class”
ridge_prob <- predict(cv.out, newx = x_test, s = lambda_1se, type = 'response')
#translate probabilities to predictions
ridge_predict <- rep('X0', nrow(test))
ridge_predict[ridge_prob>.5] <- 'X1'

#confusion matrix
table(pred=ridge_predict,true=test$Top100)
tableAccuracy(test$Top100, ridge_predict) # 0.8023715
tableTPR(test$Top100, ridge_predict) # 0.04761905
tableFPR(test$Top100, ridge_predict) # 0.007072136

# Calculate ROC curve
rocr.ridge.pred <- prediction(ridge_prob, test$Top100)
ridgePerformance <- performance(rocr.ridge.pred, "tpr", "fpr")
plot(ridgePerformance, colorize = TRUE)
abline(0, 1)

as.numeric(performance(rocr.ridge.pred, "auc")@y.values) # AUC = 0.7357884




# Model 10: Lasso Regression

set.seed(123)

# convert training data to matrix format
x <- model.matrix(Top100 ~ ., train)
# convert class to numerical variable
y <- ifelse(train$Top100=='X1',1,0)

# perform grid search to find optimal value of lambda
# family = binomial => logistic regression, alpha = 1 => lasso
# check docs to explore other type.measure options
cv.out <- cv.glmnet(x, y, alpha = 1, family = 'binomial', type.measure = 'mse')
#plot result
plot(cv.out)

# min value of lambda
lambda_min <- cv.out$lambda.min
# best value of lambda
lambda_1se <- cv.out$lambda.1se
# regression coefficients
coef(cv.out,s=lambda_1se)

# get test data
x_test <- model.matrix(Top100 ~ ., test)
# predict class, type=”class”
lasso_prob <- predict(cv.out, newx = x_test, s = lambda_1se, type = 'response')
#translate probabilities to predictions
lasso_predict <- rep('X0', nrow(test))
lasso_predict[lasso_prob>.5] <- 'X1'

#confusion matrix
table(pred=lasso_predict,true=test$Top100)
tableAccuracy(test$Top100, lasso_predict) # 0.8018069
tableTPR(test$Top100, lasso_predict) # 0.05602241
tableFPR(test$Top100, lasso_predict) # 0.00990099

# Calculate ROC curve
rocr.lasso.pred <- prediction(lasso_prob, test$Top100)
lassoPerformance <- performance(rocr.lasso.pred, "tpr", "fpr")
plot(lassoPerformance, colorize = TRUE)
abline(0, 1)

as.numeric(performance(rocr.lasso.pred, "auc")@y.values) # AUC = 0.7429794

