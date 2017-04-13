library("rpart", lib.loc="/usr/lib/R/library")

tree=rpart(factor(train$X42)~.,data = train, method="class",control = c(minsplit=10, cp=0.01))
treePred=predict(tree,newdata=test,type = c("class"))
treeTest=sum(treePred==test$X42)/nrow(test)

treeTest

plot(tree)
text(tree)
 #99.5%


start.time <- Sys.time()
treePred=predict(tree,newdata=test,type = c("class"))
treeTest=sum(treePred==test$X42)/nrow(test)

treeTest

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

confusionMatrix(treePred, test$X42)
