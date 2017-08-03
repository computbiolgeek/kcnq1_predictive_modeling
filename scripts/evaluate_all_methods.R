#!/usr/bin/env Rscript

# cross-validate optimal cutoff of SIFT
library(boot)
library(ROCR)

# load the input file
data <- read.csv(file = "dataset_all_method_evaluation.csv", header = TRUE, sep = ",")

# split the dataset into negatives and positives, drop levels that do not occur
subsets <- split(x = data, f = as.factor(data$label), drop = TRUE)
negatives <- subsets[[1]]
positives <- subsets[[2]]

compute_measures <- function(train, test) {
  # find the optimimal cut-off value from the training set
  rocr <- prediction(predictions = train$prediction, labels = train$label)
  rocr.perf <- performance(prediction.obj = rocr, measure = "mat", x.measure = "cutoff")
  optimal.cutoff.index <- which.max(unlist(rocr.perf@y.values))
  optimal.cutoff <- unlist(rocr.perf@x.values)[optimal.cutoff.index]
  
  # compute the Matthews correlation coefficient on the test set
  test$prediction <- ifelse(test$prediction <= optimal.cutoff, 0, 1)
  rocr.test <- prediction(predictions = test$prediction, labels = test$label)
  rocr.test.perf <- performance(prediction.obj = rocr.test, measure = "mat", x.measure = "cutoff")
  
  # mat.unlisted is a vector with all but one elements is NaN
  mcc.unlisted <- unlist(rocr.test.perf@y.values)
  mcc <- mcc.unlisted[2]
  ifelse(length(mcc) == 0 || mcc < 0, mccs[i, j] <- NA, mccs[i, j] <- mcc)
  
  # add tpr
  tpr.unlisted <- unlist(performance(prediction.obj = rocr.test, measure = "tpr")@y.values)
  tpr <- tpr.unlisted[2]
  
  # add tnr
  tnr.unlisted <- unlist(performance(prediction.obj = rocr.test, measure = "tnr")@y.values)
  tnr <- tnr.unlisted[2]
  
  # add ppv
  ppv.unlisted <- unlist(performance(prediction.obj = rocr.test, measure = "ppv")@y.values)
  # print(ppv.unlisted)
  ppv <- ppv.unlisted[2]
  
  # add tnr
  npv.unlisted <- unlist(performance(prediction.obj = rocr.test, measure = "npv")@y.values)
  npv <- npv.unlisted[2]
  
  # add accuracy
  acc.unlisted <- unlist(performance(prediction.obj = rocr.test, measure = "acc")@y.values)
  acc <- acc.unlisted[2]
  
  # compute  auc and store it in the numeric vector aucs
  auc <- unlist(performance(prediction.obj = rocr.test, measure = "auc")@y.values)

  # return a vector
  return(c(auc, mcc, ppv, npv, tpr, tnr, acc))
}

compute_measures_all <- function(train, test) {
  k <- ncol(train) - 1
  for(i in 1:k) {
    train.cur <- cbind(train[[i]], train[[k+1]])
    head(train.cur)
    colnames(train.cur) <- c("prediction", "label")
    #measures <- compute_measures(train = train.cur, test = test)
    #print(measures)
  }
}


# set random seed
set.seed(5255599)
for(i in 1:2) {
  # shuffle each subset
  negatives <- negatives[sample(x = 1:nrow(negatives)),]
  positives <- positives[sample(x = 1:nrow(positives)),]
  
  # create folds
  neg.folds <- cut(x = 1:nrow(negatives), breaks = 3, labels = FALSE)
  pos.folds <- cut(x = 1:nrow(positives), breaks = 3, labels = FALSE)
  for(j in 1:3) {
    # create test set and training set
    test <- rbind(negatives[neg.folds == j,], positives[pos.folds == j,])
    train <- rbind(negatives[neg.folds != j,], positives[pos.folds != j,])
    
    compute_measures_all(train, test)
  }
}
