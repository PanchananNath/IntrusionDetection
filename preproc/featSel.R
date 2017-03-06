cfs(X42~. , KDDTrain_20Percent)

random.forest.importance(X42~. , KDDTrain_20Percent , importance.type = 1)

bor=Boruta(factor(X42)~. , KDDTrain_20Percent)