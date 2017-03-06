library("e1071", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
svmMod=svm(factor(train$X42)~., train)
svmpred=predict(svmMod,test)
svmAcc=sum(svmpred==test$X42)/nrow(test)
svmAcc

#97.3%