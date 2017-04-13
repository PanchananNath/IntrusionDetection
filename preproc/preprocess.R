

for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]==101))
    KDDTrain_20Percent$X42[i]="DOS"
  
  for(i in 1:nrow(KDDTrain_20Percent))
    if((KDDTrain_20Percent$X42[i]==102))
      KDDTrain_20Percent$X42[i]="Probing"
    for(i in 1:nrow(KDDTrain_20Percent))
      if((KDDTrain_20Percent$X42[i]==103))
        KDDTrain_20Percent$X42[i]="R2L"
      for(i in 1:nrow(KDDTrain_20Percent))
        if((KDDTrain_20Percent$X42[i]==104))
          KDDTrain_20Percent$X42[i]="U2R"
        for(i in 1:nrow(KDDTrain_20Percent))
          if((KDDTrain_20Percent$X42[i]==105))
            KDDTrain_20Percent$X42[i]="normal"

          
         
          