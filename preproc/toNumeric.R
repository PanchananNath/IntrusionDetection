
numer =  function(x) {
x <- as.factor(x)
levels(x) <- 1:length(levels(x))
x <- as.numeric(x)
return(x)
}

KDDTrain_20Percent$X2 = numer(KDDTrain_20Percent$X2)
KDDTrain_20Percent$X3 = numer(KDDTrain_20Percent$X3)
KDDTrain_20Percent$X4 = numer(KDDTrain_20Percent$X4)

for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]!="normal"))
    KDDTrain_20Percent$X42[i]=1

for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]=="normal"))
    KDDTrain_20Percent$X42[i]=0




KDDTrain_20Percent=KDDTrain_20Percent[-43]
