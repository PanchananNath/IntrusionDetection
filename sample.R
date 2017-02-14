## 75% of the sample size
smp_size <- floor(0.75 * nrow(KDDTrain_20Percent))

## set the seed to make your partition reproductible
#set.seed(123)
train_ind <- sample(seq_len(nrow(KDDTrain_20Percent)), size = smp_size)

train <- KDDTrain_20Percent[train_ind, ]
test <- KDDTrain_20Percent[-train_ind, ]