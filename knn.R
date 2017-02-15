library("FNN", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
ac=array(dim=sqrt(nrow(train)))
nn=array(dim=sqrt(nrow(train)))
for(i in 2:sqrt(nrow(train))){
knn=knn(train,test,factor(train$X42),k=i)
knnAcc=sum(knn==test$X42)/nrow(test)
ac = c(ac,knnAcc)
nn=c(nn,i)
}

plot(ac,nn, xlab = "accuracy", ylab="value of k")
#99.01%  98.5 98.1 multi