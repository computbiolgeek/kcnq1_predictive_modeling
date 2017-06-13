# load required libraries
library(ROCR)

# load data set
kcnq_df <- read.csv(file = "training_dataset.csv", header = TRUE, stringsAsFactors = FALSE)

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
# set.seed(seed = 111111)
for(i in 1:r) {
  neg.folds <- cut(x = 1:nrow(negatives), breaks = k, labels = FALSE)
  pos.folds <- cut(x = 1:nrow(positives), breaks = k, labels = FALSE)
  for(j in 1:k) {
    # test set
    test <- rbind(negatives[neg.folds == j,], positives[pos.folds == j,])
    # training set
    train <- rbind(negatives[neg.folds !=j,], positives[pos.folds != j,])
    # logistic regression model
    logit.model <- glm(label ~ pssm + erate, data = train, family = "binomial")
    # make predictions on the test set
    test$logit.pred <- predict(object = logit.model, newdata = test, type = "response")
    # compute area under the ROC curve, or AUC
    rocr <- prediction(predictions = test$logit.pred, labels = test$label)
    perf <- performance(prediction.obj = rocr, measure = "auc")
    aucs[(i-1)*k+j] <- unlist(perf@y.values)
  }
}

# print the mean AUC
print(mean(aucs))