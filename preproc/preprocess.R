

for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]!="normal"))
    KDDTrain_20Percent$X42[i]=1
  
for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]=="normal"))
    KDDTrain_20Percent$X42[i]=0



    
KDDTrain_20Percent=KDDTrain_20Percent[-43]
  