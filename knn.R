library("FNN", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

knn=knn(train,test,factor(train$X42),k=3)
knnAcc=sum(knn==test$X42)/nrow(test)
knnAcc

#99.01%  98.5 98.1 multi