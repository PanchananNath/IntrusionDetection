unique(KDDTrain_20Percent$X42)
uni<-unique(KDDTrain_20Percent$X42)
count<-table(KDDTrain_20Percent$X42)

attacktype = as.data.frame(count)

numer =  function(x) {
  x <- as.factor(x)
  levels(x) <- 1:length(levels(x))
  x <- as.numeric(x)
  return(x)
}

KDDTrain_20Percent$X2 = numer(KDDTrain_20Percent$X2)
KDDTrain_20Percent$X3 = numer(KDDTrain_20Percent$X3)
KDDTrain_20Percent$X4 = numer(KDDTrain_20Percent$X4)
KDDTrain_20Percent$X42 = numer(KDDTrain_20Percent$X42)

for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]== 1 ||KDDTrain_20Percent$X42[i]== 7||KDDTrain_20Percent$X42[i]== 10|| KDDTrain_20Percent$X42[i]== 14||KDDTrain_20Percent$X42[i]== 18||KDDTrain_20Percent$X42[i]== 20))
    KDDTrain_20Percent$X42[i]=1


for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]== 17 ||KDDTrain_20Percent$X42[i]== 6||KDDTrain_20Percent$X42[i]== 11|| KDDTrain_20Percent$X42[i]== 15))
    KDDTrain_20Percent$X42[i]=2

for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]== 4 ||KDDTrain_20Percent$X42[i]== 3||KDDTrain_20Percent$X42[i]== 5|| KDDTrain_20Percent$X42[i]== 13||  KDDTrain_20Percent$X42[i]== 9 ||  KDDTrain_20Percent$X42[i]== 22 ||  KDDTrain_20Percent$X42[i]== 21 ||  KDDTrain_20Percent$X42[i]== 19))
    KDDTrain_20Percent$X42[i]=3



for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]== 2||KDDTrain_20Percent$X42[i]== 8||KDDTrain_20Percent$X42[i]== 16))
    KDDTrain_20Percent$X42[i]=4

for(i in 1:nrow(KDDTrain_20Percent))
  if((KDDTrain_20Percent$X42[i]== 12))
    KDDTrain_20Percent$X42[i]=5

KDDTrain_20Percent=KDDTrain_20Percent[-43]
