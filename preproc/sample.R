## 75% of the sample size
smp_size <- floor(0.50 * nrow(KDDTrain_20Percent))

## set the seed to make your partition reproductible
#set.seed(123)
train_ind <- sample(seq_len(nrow(KDDTrain_20Percent)), size = smp_size)

train <- KDDTrain_20Percent[train_ind, ]
test <- KDDTrain_20Percent[-train_ind, ]

doit <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                 min(x, na.rm=TRUE))}

# use lapply to apply doit() to every column in a data frame
# mtcars is built into R
normed <- as.data.frame(lapply(KDDTrain_20Percent, doit))
# very that the range of all is [0, 1]
lapply(normed, range)
normed=normed[-c(20,21,42)]
normed$X42 <- KDDTrain_20Percent$X42

## 75% of the sample size
smp_size <- floor(0.50 * nrow(normed))

## set the seed to make your partition reproductible
#set.seed(123)
train_ind <- sample(seq_len(nrow(normed)), size = smp_size)

train <- normed[train_ind, ]
test <- normed[-train_ind, ]