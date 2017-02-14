library("e1071")
naive=naiveBayes(factor(X42)~.,data = train)
naivepred=predict(naive,test[-42],threshold = 0.1)
naiveAcc=sum(naivepred==test$X42)/nrow(test)
naiveAcc
 
#88.6%
#75 multiclass