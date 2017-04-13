library("e1071")
naive=naiveBayes(factor(X42)~.,data = train)
naivepred=predict(naive,test[-42],threshold = 0.1)
naiveAcc=sum(naivepred==test$X42)/nrow(test)
naiveAcc
 


start.time <- Sys.time()
naivepred=predict(naive,test[-42],threshold = 0.1)
naiveAcc=sum(naivepred==test$X42)/nrow(test)
naiveAcc
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
confusionMatrix(naivepred, test$X42)
