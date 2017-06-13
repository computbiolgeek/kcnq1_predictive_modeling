library(Hmisc)

# load data set
kcnq_df <- read.csv(file = "dataset_all_included.csv", header = TRUE, stringsAsFactors = FALSE)
kcnq_df$label <- as.factor(ifelse(kcnq_df$label == 0, "Normal", "Dysfunctional"))

# determine a logistic regression boundary
# logis.model <- glm(label ~ pssm + erate, family = binomial, data = kcnq_df)
coefs <- c(-1.5077062, -0.2600895, 0.9694561)
intercept <- -(coefs[1] / coefs[2])

# Scatter plot of variants
png(filename = "pssm_erate_scatter_plot.png", width = 8, height = 8, units = "in", res = 600)
op <- par(mar = c(5, 6, 4, 2) + 0.1)
plot(
  x = kcnq_df$erate,
  y = kcnq_df$pssm,
  xlim = c(0.0, 6.0),
  ylim = c(-5.0, 20.0),
  xlab = "Rate of evolution",
  ylab = "",
  cex.axis = 1.5, cex.lab = 1.5, font.lab = 2, pch = 21, cex = 2,
  col = "black", bg = c("salmon3", "green3")[as.numeric(kcnq_df$label)],
  lwd = 2, las = 1
)
title(ylab = "PSSM-derived perturbation", line = 3, cex.lab = 1.5, font.lab = 2)
minor.tick(nx = 2, ny = 2, tick.ratio = 0.5)
segments(x0 = 0, y0 = intercept, x1 = 5.0, y1 = 12.84, lwd = 2)
text(x = 3.3, y = 10, "Decision boundary", cex = 1.2)
box(which = "plot", lwd = 2)
legend(x = 4, y = 17, cex = 1.2,
       legend = c("Dysfunctional", "Normal"), 
       pch = 21, pt.bg = c("salmon3", "green3"), 
       border = "black")
par(op)
dev.off()