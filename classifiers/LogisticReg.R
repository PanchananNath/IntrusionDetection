library("glmnet", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

logisMod=glmnet(as.matrix(KDDTrain_20Percent[c(-2,-3,-4,-42)]),as.factor(KDDTrain_20Percent$X42),family = "multinomial")
logisPred=predict(logisMod,type=c("class"),newx=as.matrix(KDDTest_[c(-2,-3,-4,-42)]))

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)