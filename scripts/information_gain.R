#!/usr/bin/env Rscript

# information gain
library(argparser)

# creat a argparser object for parsing command line arguments
args_parser <- arg_parser(description = "This script computes information gain for a feature")
args_parser <- add_argument(parser = args_parser, arg = "--input", help = "input file")
args_parser <- add_argument(parser = args_parser, arg = "--output", help = "output file")
args_parser <- add_argument(parser = args_parser, arg = "--categorical", help = "categorial feature?", default = "no")
args_parser <- add_argument(parser = args_parser, arg = "--header", help = "has headers?", default = "no")
args <- parse_args(parser = args_parser)

# load data set
df <- read.csv(file = args$input, header = ifelse(args$header %in% c("y", "yes", "Y", "Yes"), TRUE, FALSE), stringsAsFactors = TRUE)
colnames(df) <- c("feature", "label")

# function that computes entropy
entropy <- function(x) {
  if(!is.factor(x)) {
    warning("given object is not a factor object, attempt was made to convert it to a factor")
    x <- as.factor(x)
  }
  p <- table(x) / length(x)
  return(-sum(ifelse(p == 0 | p == 1, 0, p * log2(p))))
}

# compute information gain
emp_ent <- entropy(df$label) # empirical entropy

# if the feature is categorical
if(args$categorical %in% c("y", "yes", "Y", "Yes")) {
  dfs <- split(x = df, f = df$feature)
  ents <- numeric(length = length(dfs))
  for(i in 1:length(dfs)) {
    ents[i] <- entropy(dfs[[i]]$label)
  }
  cond_ent <- sum((table(df$feature) / nrow(df)) * ents)
  info_gain <- emp_ent - cond_ent
  info_gain_ratio <- info_gain / entropy(df$feature)
  write.csv(x = data.frame(info_gain, info_gain_ratio), file = args$output, row.names = FALSE)
} else { # if the feature is not categorical
  n_rows <- nrow(df)
  cond_ents <- numeric(length = n_rows)
  feature_ents <- numeric(length = n_rows)
  df_sorted <- df[order(df$feature),]
  for(i in 1:n_rows) {
    cond_ents[i] <- i / n_rows * entropy(df_sorted[1:i, "label"]) + 
      (1 - i / n_rows) * entropy(df_sorted[(i+1):n_rows, "label"])
    feature_ents[i] <- entropy(1:n_rows <= i)
  }
  info_gain <- emp_ent - cond_ents
  info_gain_ratio <- info_gain / feature_ents
  info_gain_df <- data.frame(threshold = df_sorted$feature, info_gain = info_gain, info_gain_ratio = info_gain_ratio)
  write.csv(x = info_gain_df, file = args$output, row.names = FALSE)
}