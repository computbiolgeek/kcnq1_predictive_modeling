# load required libraries
library(ROCR)

# set working directory to where the data is stored
setwd("/dors/meilerlab/home/lib14/projects/kcnq1/modeling/vu_collection/hidden1neuron_nodropout/")

replicates <- 30
folds <- 3
aucs.test <- mccs.test <- ppvs.test <- npvs.test <- tprs.test <- tnrs.test <- accs.test <- numeric(length = replicates)
for (i in 1:replicates) {
  aucs.test.folds <- numeric(length = folds)
  mccs.test.folds <- numeric(length = folds)
  ppvs.test.folds <- numeric(length = folds)
  npvs.test.folds <- numeric(length = folds)
  tprs.test.folds <- numeric(length = folds)
  tnrs.test.folds <- numeric(length = folds)
  accs.test.folds <- numeric(length = folds)
  for (j in 0:(folds - 1)) {
    cat("reading", paste("training_",i,"_",j,".csv",sep = ""),"\n")
    df.train <- read.csv(file = paste("training_",i,"_",j,".csv",sep = ""),
                         header = FALSE)
    cat("reading", paste("testing_",i,"_",j,".csv",sep = ""),"\n")
    df.test <- read.csv(file = paste("testing_",i,"_",j,".csv",sep = ""),
                        header = FALSE)
    # create a ROCR prediction object
    # the first four columns are: sequence id, wilt type, mutant, and label
    rocr.train <- prediction(predictions = df.train[, 2], labels = df.train[, 3])
    # compute current mcc and store it in the numeric vector mccs
    mats.unlisted <- unlist(performance(prediction.obj = rocr.train, measure = "mat")@y.values)
    max.mcc.index <- which.max(mats.unlisted)
    # optimal cutoff
    optimal_cutoff <- unlist(rocr.train@cutoffs)[max.mcc.index]
    
    
    rocr.test <- prediction(predictions = df.test[, 2], labels = df.test[, 3])
    # compute current auc and store it in the numeric vector aucs
    aucs.test.folds[j+1] <- unlist(performance(prediction.obj = rocr.test, measure = "auc")@y.values)
    
    # classify testing set
    df.test[, 2] <- ifelse(df.test[, 2] < optimal_cutoff, 0, 1)
    rocr.test.new <- prediction(predictions = df.test[, 2], labels = df.test[, 3])
    # compute current mcc and store it in the numeric vector mccs
    mats.unlisted <- unlist(performance(prediction.obj = rocr.test.new, measure = "mat")@y.values)
    mccs.test.folds[j+1] <- mats.unlisted[2]
    tprs.unlisted <- unlist(performance(prediction.obj = rocr.test.new, measure = "tpr")@y.values)
    tprs.test.folds[j + 1] <- tprs.unlisted[2]
    tnrs.unlisted <- unlist(performance(prediction.obj = rocr.test.new, measure = "tnr")@y.values)
    tnrs.test.folds[j + 1] <- tnrs.unlisted[2]
    ppvs.unlisted <- unlist(performance(prediction.obj = rocr.test.new, measure = "ppv")@y.values)
    ppvs.test.folds[j + 1] <- ppvs.unlisted[2]
    npvs.unlisted <- unlist(performance(prediction.obj = rocr.test.new, measure = "npv")@y.values)
    npvs.test.folds[j + 1] <- npvs.unlisted[2]
    accs.unlisted <- unlist(performance(prediction.obj = rocr.test.new, measure = "acc")@y.values)
    accs.test.folds[j + 1] <- accs.unlisted[2]
  }
  aucs.test[i] <- mean(aucs.test.folds)
  mccs.test[i] <- mean(mccs.test.folds)
  ppvs.test[i] <- mean(tprs.test.folds)
  npvs.test[i] <- mean(tnrs.test.folds)
  tprs.test[i] <- mean(tprs.test.folds)
  tnrs.test[i] <- mean(tnrs.test.folds)
  accs.test[i] <- mean(accs.test.folds)
}