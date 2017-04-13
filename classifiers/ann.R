library("nnet", lib.loc="/usr/lib/R/library")

annMod=nnet(factor(train$X42)~.  , train[c(5,3,30,4,29,6,35,38,23,34)],decay=0.0001, size=20, maxit=100)
annpred=predict(annMod,test)
annpred
annAcc=sum(round(annpred)==test$X42)/nrow(test)
annAcc

#table(round(annpred), test$X42)
confusionMatrix(annpred, test$X42)
#94.7

