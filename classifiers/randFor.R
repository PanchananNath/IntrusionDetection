library("randomForest", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

randMod=randomForest(factor(train$X42)~. , train, ntree=500, mtry = floor(sqrt(ncol(train)) ))
randpred=predict(randMod,test)
randAcc=sum(randpred==test$X42)/nrow(test)
randAcc

#99.7 98.7
save(randMod, file = "RandFor.rda")
confusionMatrix(randpred, test$X42)

start.time <- Sys.time()
randpred=predict(randMod,test)
randAcc=sum(randpred==test$X42)/nrow(test)
randAcc

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
