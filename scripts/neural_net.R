#########################################################
# @brief Fit neural net to the data set
# @details Completely rewritten from the previous version.
# Bootstrap is used for estimating optimism about AUC
# @version 1.1
# @author Bian Li
# @date 10/15/2015
#########################################################

# load required libraries
library(nnet)
library(ROCR)

# load data set
datafile <- "training_dataset.csv"
kcnq_df <- read.csv(file = datafile, header = TRUE, stringsAsFactors = FALSE)

# rename the last column
colnames(kcnq_df)[ncol(kcnq_df)] <- c("label")

#############################################################################
# Fit a neural net to the original data set
#############################################################################

hidden.layer <- 3
# fit a neural net to the whole data set
orig_net <- nnet(label ~ pssm + erate, data = kcnq_df, size = hidden.layer, decay = 0.02, maxit = 1000)
# predictions on the original data set
orig_predictions <- predict(orig_net, newdata = kcnq_df, type = "raw")
# create a ROCR prediction object
orig_rocr <- prediction(predictions = orig_predictions, labels = kcnq_df$label)
# compute the apparent AUC
AUC_app <- unlist(performance(orig_rocr, measure = "auc")@y.values)
# save the model for reuse
save(orig_net, file = "kcnq1_nnet.rda")

########################
# Bootstrap AUC
########################

# number of boostrap iterations
itrs <- 200
AUC_bts <- numeric(length = itrs)
AUC_bt_origs <- numeric(length = itrs)
AUC_optimisms <- numeric(length = itrs)
for (i in 1:itrs) {
  # sample a bootstrap data set
  bt_indices <- sample(1:nrow(kcnq_df), nrow(kcnq_df), replace = TRUE)
  bt_set <- kcnq_df[bt_indices, ]
  # fit a neural net with the same architecture to the bootstrap data set
  bt_net <- nnet(label ~ pssm + erate, data = bt_set, size = hidden.layer, decay = 0.02, maxit = 1000)
  # predictions on the bootstrap data set
  bt_predictions <- predict(bt_net, newdata = bt_set, type = "raw")
  # create a ROCR prediction object
  rocr_bt <- prediction(predictions = bt_predictions, labels = bt_set$label)
  # compute the AUC from the bootstrap data set
  AUC_bts[i] <- unlist(performance(rocr_bt, measure = "auc")@y.values)
  # use the bootstrap neural net to predict the original data set
  predictions_bt_orig <- predict(bt_net, newdata = kcnq_df, type = "raw")
  # create a ROCR prediction object
  rocr_bt_orig <- prediction(predictions = predictions_bt_orig, labels = kcnq_df$label)
  # compute the AUC from the bootstrap data set
  AUC_bt_origs[i] <- unlist(performance(rocr_bt_orig, measure = "auc")@y.values)
  # push back current optimism about AUC
  AUC_optimisms[i] <- AUC_bts[i] - AUC_bt_origs[i]
}

#################################################
# Compute the optimism adjusted AUC
#################################################

AUC_mean_optimism <- mean(AUC_optimisms)
AUC_adj <- AUC_app - AUC_mean_optimism