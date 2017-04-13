library("readr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
KDDTrain_20Percent <- read_csv("~/fyp/CTD/KDDTrain+_20Percent.txt", col_names = FALSE)

source('~/fyp/CTD/preproc/TrainMap.R', echo=TRUE)

source('~/fyp/CTD/preproc/sample.R', echo=TRUE)
