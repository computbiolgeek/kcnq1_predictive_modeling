# load required libraries
library(nnet)
library(ROCR)

# load data set
datafile <- "training_dataset.csv"
kcnq_df <- read.csv(file = datafile, header = TRUE, stringsAsFactors = FALSE)

# split the dataset into negatives and positives, drop levels that do not occur
subsets <- split(x = kcnq_df, f = as.factor(kcnq_df$label), drop = TRUE)
negatives <- subsets[[1]]
positives <- subsets[[2]]

# shuffle the subsets
negatives <- negatives[sample(1:nrow(negatives)),]
positives <- positives[sample(1:nrow(positives)),]

# repeated k-fold cross-validation
r <- 200
k <- 3
aucs <- numeric(length = k)
hidden.layer <- 3
set.seed(seed = 111111)
for(i in 1:r) {
  neg.folds <- cut(x = 1:nrow(negatives), breaks = k, labels = FALSE)
  pos.folds <- cut(x = 1:nrow(positives), breaks = k, labels = FALSE)
  for(j in 1:k) {
    # test set
    test <- rbind(negatives[neg.folds == j,], positives[pos.folds == j,])
    # training set
    train <- rbind(negatives[neg.folds !=j,], positives[pos.folds != j,])
    # logistic regression model
    nnet.model <- nnet(label ~ pssm + erate, data = train, size = hidden.layer, decay = 0.02, maxit = 1000)
    # make predictions on the test set
    test$nnet.pred <- predict(object = nnet.model, newdata = test, type = "raw")
    # compute area under the ROC curve, or AUC
    rocr <- prediction(predictions = test$nnet.pred, labels = test$label)
    perf <- performance(prediction.obj = rocr, measure = "auc")
    aucs[(i-1)*k+j] <- unlist(perf@y.values)
  }
}

# print the mean AUC
print(mean(aucs))