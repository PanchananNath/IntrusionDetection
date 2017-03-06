library("rpart", lib.loc="/usr/lib/R/library")

tree=rpart(factor(train$X42)~.,data = train, control = rpart.control(minsplit = 4,cp=0))
treePred=predict(tree,newdata=test,type = c("class"))
treeTest=sum(treePred==test$X42)/nrow(test)

treeTest

plot(tree)
 #99.5%