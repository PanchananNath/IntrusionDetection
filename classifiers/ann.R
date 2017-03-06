library("nnet", lib.loc="/usr/lib/R/library")

annMod=nnet(factor(train$X42)~.  , train[c(4,5,6,12,26,29,30,37)],decay=0.0001, size=20, maxit=100)
annpred=predict(annMod,test)
annpred
annAcc=sum(round(annpred)==test$X42)/nrow(test)
annAcc

table(annpred, test$X42)

#94.7