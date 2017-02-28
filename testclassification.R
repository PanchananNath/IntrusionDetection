unique(KDDTest_21$X42)
uni<-unique(KDDTest_21$X42)
count<-table(KDDTest_21$X42)

attacktype1 = as.data.frame(count)

numer =  function(x) {
  x <- as.factor(x)
  levels(x) <- 1:length(levels(x))
  x <- as.numeric(x)
  return(x)
}

KDDTest_21$X2 = numer(KDDTest_21$X2)
KDDTest_21$X3 = numer(KDDTest_21$X3)
KDDTest_21$X4 = numer(KDDTest_21$X4)
KDDTest_21$X42 = numer(KDDTest_21$X42)

for(i in 1:nrow(KDDTest_21))
  if((KDDTest_21$X42[i]== 1 ||KDDTest_21$X42[i]== 2||KDDTest_21$X42[i]== 15|| KDDTest_21$X42[i]== 20||KDDTest_21$X42[i]== 28||KDDTest_21$X42[i]== 32 || KDDTest_21$X42[i]== 33 || KDDTest_21$X42[i]== 22 ||KDDTest_21$X42[i]== 35 || KDDTest_21$X42[i]== 9))
    KDDTest_21$X42[i]=1


for(i in 1:nrow(KDDTest_21))
  if((KDDTest_21$X42[i]== 26 ||KDDTest_21$X42[i]== 8||KDDTest_21$X42[i]== 16|| KDDTest_21$X42[i]== 21 ||KDDTest_21$X42[i]== 12 || KDDTest_21$X42[i]== 25))
    KDDTest_21$X42[i]=2

for(i in 1:nrow(KDDTest_21))
  if((KDDTest_21$X42[i]== 5 ||KDDTest_21$X42[i]== 4||KDDTest_21$X42[i]== 7|| KDDTest_21$X42[i]== 19||  KDDTest_21$X42[i]== 13 ||  KDDTest_21$X42[i]== 34 ||  KDDTest_21$X42[i]== 36||  KDDTest_21$X42[i]== 37 || KDDTest_21$X42[i]== 30 || KDDTest_21$X42[i]== 29 || KDDTest_21$X42[i]== 6 || KDDTest_21$X42[i]== 27 || KDDTest_21$X42[i]== 14))
    KDDTest_21$X42[i]=3



for(i in 1:nrow(KDDTest_21))
  if((KDDTest_21$X42[i]== 3||KDDTest_21$X42[i]== 10||KDDTest_21$X42[i]== 24||KDDTest_21$X42[i]== 18 || KDDTest_21$X42[i]== 31 || KDDTest_21$X42[i]== 38 || KDDTest_21$X42[i]== 23))
    KDDTest_21$X42[i]=4

for(i in 1:nrow(KDDTest_21))
  if((KDDTest_21$X42[i]== 12))
    KDDTest_21$X42[i]=5

KDDTest_21=KDDTest_21[-43]
