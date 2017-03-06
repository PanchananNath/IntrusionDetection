library("randomForest", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

randMod=randomForest(factor(train$X42)~.  , train[-1])
randpred=predict(randMod,test)
randAcc=sum(randpred==test$X42)/nrow(test)
randAcc

#99.7 98.7