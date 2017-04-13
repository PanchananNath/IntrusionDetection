# Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
fit.treebag <- train(factor(KDDTrain_20Percent$X42)~., data=KDDTrain_20Percent, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(factor(KDDTrain_20Percent$X42)~., data=KDDTrain_20Percent, method="rf", metric=metric, trControl=control)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)