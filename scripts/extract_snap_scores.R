# extract SNAP scores

# load training set
df <- read.csv(file = "training_dataset.csv", header = TRUE, stringsAsFactors = FALSE)

# the list of variants
variants <- paste(df$w, df$seq_pos, df$m, sep = "")

# load all SNAP scores
snap_scores <- read.csv(file = "snap_scores_all.csv", header = TRUE)
rownames(snap_scores) <- snap_scores$Variant

# get SNAP scores for training set
snap_scores_training_set <- snap_scores[variants, ]
snap_scores_training_set$Label <- df$label

# write to file
write.csv(x = snap_scores_training_set, file = "snap_scores_training_set.csv", row.names = FALSE)
