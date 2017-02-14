svmMod=svm(factor(train$X42)~., train)
svmpred=predict(svmMod,test)
svmAcc=sum(svmpred==test$X42)/nrow(test)
svmAcc

#97.3%