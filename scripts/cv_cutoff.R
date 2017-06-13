#!/usr/bin/env Rscript

# cross-validate optimal cutoff of SIFT
library(ROCR)
library(argparser)

# create an arg.parser object for parsing command line arguments
args.parser <- arg_parser(description = "This script takes a list of (prediction, label) pairs and does a 
                          k-fold cross validation of the procedure for choosing optimimal cut-off based on MCC",
                          name = "cv_cutoff")
args.parser <- add_argument(parser = args.parser, arg = "-i", help = "input file")
args.parser <- add_argument(parser = args.parser, arg = "-k", help = "number of folds", default = 3)
args.parser <- add_argument(parser = args.parser, arg = "-r", help = "number of repeats", default = 1)

# parse command line arguments
argv <- parse_args(parser = args.parser)

# load the input file
data <- read.csv(file = argv$i, header = FALSE, sep = ",")
colnames(data) <- c("prediction", "label")

# split the dataset into negatives and positives, drop levels that do not occur
subsets <- split(x = data, f = as.factor(data$label), drop = TRUE)
negatives <- subsets[[1]]
positives <- subsets[[2]]

# do the cross validation
vec <- numeric(length = argv$r * argv$k)
mat <- matrix(data = vec, nrow = argv$r, ncol = argv$k, byrow = TRUE)
tprs <- tnrs <- accs <- mccs <- aucs <- mat

for(i in 1:argv$r) {
  # shuffle each subsets
  negatives <- negatives[sample(x = 1:nrow(negatives)),]
  positives <- positives[sample(x = 1:nrow(positives)),]
  
  # create folds
  neg.folds <- cut(x = 1:nrow(negatives), breaks = argv$k, labels = FALSE)
  pos.folds <- cut(x = 1:nrow(positives), breaks = argv$k, labels = FALSE)
  for(j in 1:argv$k) {
    # create test set and training set
    test <- rbind(negatives[neg.folds == j,], positives[pos.folds == j,])
    train <- rbind(negatives[neg.folds != j,], positives[pos.folds != j,])
    
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
    tprs[i, j] <- tpr.unlisted[2]
    
    # add tnr
    tnr.unlisted <- unlist(performance(prediction.obj = rocr.test, measure = "tnr")@y.values)
    tnrs[i, j] <- tnr.unlisted[2]
    
    # add accuracy
    acc.unlisted <- unlist(performance(prediction.obj = rocr.test, measure = "acc")@y.values)
    accs[i, j] <- acc.unlisted[2]
    
    # compute  auc and store it in the numeric vector aucs
    auc <- unlist(performance(prediction.obj = rocr.test, measure = "auc")@y.values)
    aucs[i, j] <- auc
  }
}

# print the matrix
tprs <- cbind(tprs, apply(tprs, MARGIN = 1, FUN = mean))
tnrs <- cbind(tnrs, apply(tnrs, MARGIN = 1, FUN = mean))
accs <- cbind(accs, apply(accs, MARGIN = 1, FUN = mean))
mccs <- cbind(mccs, apply(mccs, MARGIN = 1, FUN = mean))
aucs <- cbind(aucs, apply(aucs, MARGIN = 1, FUN = mean))
print(mean(tprs[, 4], na.rm = TRUE))
print(mean(tnrs[, 4], na.rm = TRUE))
print(mean(accs[, 4], na.rm = TRUE))
print(mean(mccs[, 4], na.rm = TRUE))
print(mean(aucs[, 4], na.rm = TRUE))
