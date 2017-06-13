# load libraries required by the analysis
library(ROCR)
library(argparser)

# create an argparser object for parsing command line arguments
args.parser <- arg_parser(description = "This script takes a list of prediction, label pairs and computes 
                          performance measures at each possible thresholds. The results are stored in a data frame")
args.parser <- add_argument(parser = args.parser, arg = "-i", help = "input file")
args.parser <- add_argument(parser = args.parser, arg = "-o", help = "output file")

# parse command line arguments
argv <- parse_args(parser = args.parser)

# load data
df <- read.csv(file = argv$i, header = FALSE, stringsAsFactors = TRUE)
colnames(df) <- c("prob", "label")

# create a ROCR prediction object the first four columns are: res_id, wild_type, variant, prob, and label
rocr.prediction <- prediction(predictions = df$prob, labels = df$label)

# compute  auc and store it in the numeric vector aucs
auc <- unlist(performance(prediction.obj = rocr.prediction, measure = "auc")@y.values)

# compute mcc and store it in the numeric vector mccs
mats.unlisted <- unlist(performance(prediction.obj = rocr.prediction, measure = "mat")@y.values)

# add tpr
tpr.unlisted <- unlist(performance(prediction.obj = rocr.prediction, measure = "tpr")@y.values)

# add tnr
tnr.unlisted <- unlist(performance(prediction.obj = rocr.prediction, measure = "tnr")@y.values)

# add ppv
ppv.unlisted <- unlist(performance(prediction.obj = rocr.prediction, measure = "ppv")@y.values)

# add npv
npv.unlisted <- unlist(performance(prediction.obj = rocr.prediction, measure = "npv")@y.values)

# add accuracy
acc.unlisted <- unlist(performance(prediction.obj = rocr.prediction, measure = "acc")@y.values)

# creaet a data frame that contains all performance measures
performance_measures <- data.frame(cutoff = rocr.prediction@cutoffs, tpr = tpr.unlisted, tnr = tnr.unlisted,
                                   ppv = ppv.unlisted, npv = npv.unlisted, acc = acc.unlisted)

# write to output file
print(auc)
write.csv(x = performance_measures, file = argv$o, row.names = FALSE)
