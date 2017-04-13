# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
data(PimaIndiansDiabetes)
# load the data
KDDTrain_20Percent = as.data.frame(table(unlist(KDDTrain_20Percent)))
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train[,1:41], factor(train[,42]), sizes=c(1:10), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

