setwd("//media//kswada//MyFiles//R//cell_segmentation")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cell Segmentation in High-Content Screening
# ------------------------------------------------------------------------------
data("segmentationOriginal", package = "AppliedPredictiveModeling")


# Retain the original training set
segTrain <- subset(segmentationOriginal, Case == "Train")


# Remove the first three columns (identifier columns)
segTrainX <- segTrain[, -(1:3)]
segTrainClass <- segTrain$Class



# ------------------------------------------------------------------------------
# data reduction and feature extraction by principal component analysis
# ------------------------------------------------------------------------------
# This set contains a subset of two correlated predictors, average pixel intensity of channel 1 and entropy of intensity values in the cell
# (a measure of cell shape), and a categorical response.

with(segTrainTrans, cor(AvgIntenCh1, EntropyIntenCh1))



# --> 
# Given the high correlation between the predictors (0.93), we could infer that average pixel intensity and entropy of intensity values measure
# redundant information about the cells and that eigher predictor or a linear combination of these predictors could be used in place of the original predictors


pr <- prcomp(~ AvgIntenCh1 + EntropyIntenCh1, 
             data = segTrainTrans, 
             scale. = TRUE)


pr


pr$sdev^2 / sum(pr$sdev^2)


# -->
# The 1st PC summarizes 97% of the original variability, while the second summarizes 3%


# ----------
transparentTheme(pchSize = .7, trans = .3)


xyplot(AvgIntenCh1 ~ EntropyIntenCh1,
       data = segTrainTrans,
       groups = segTrain$Class,
       xlab = "Channel 1 Fiber Width",
       ylab = "Intensity Entropy Channel 1",
       auto.key = list(columns = 2),
       type = c("p", "g"),
       main = "Original Data",
       aspect = 1)


xyplot(PC2 ~ PC1,
       data = as.data.frame(pr$x),
       groups = segTrain$Class,
       xlab = "Principal Component #1",
       ylab = "Principal Component #2",
       main = "Transformed",
       xlim = extendrange(pr$x),
       ylim = extendrange(pr$x),
       type = c("p", "g"),
       aspect = 1)



# ------------------------------------------------------------------------------
# Check between-predictors correlation and assume how many significant relationships between the predictors
# ------------------------------------------------------------------------------
segCorr <- cor(segTrainTrans)

library(corrplot)
corrplot(segCorr, order = "hclust", tl.cex = .35)



# --> 
# This would indicate that there are at least 3-4 significant relationships between the predictors.



# ------------------------------------------------------------------------------
# Apply PCA to the entire set of predictors
# ------------------------------------------------------------------------------
# There are a few predictors with only a single value, so we remove these first
# (since PCA uses variances, which would be zero)

isZV <- apply(segTrainX, 2, function(x) length(unique(x)) == 1)

segTrainX <- segTrainX[, !isZV]



# ----------
segPP <- preProcess(segTrainX, c("BoxCox", "center", "scale"))

segTrainTrans <- predict(segPP, segTrainX)

segPCA <- prcomp(segTrainTrans, center = TRUE, scale. = TRUE)


cumsum(round(segPCA$sdev^2 / sum(segPCA$sdev^2), digits = 3)[1:10])



# ----------
# Plot a scatterplot matrix of the first three components
transparentTheme(pchSize = .8, trans = .3)

panelRange <- extendrange(segPCA$x[, 1:3])
panelRange


splom(as.data.frame(segPCA$x[, 1:3]),
      groups = segTrainClass,
      type = c("p", "g"),
      as.table = TRUE,
      auto.key = list(columns = 2),
      prepanel.limits = function(x) panelRange)



# -->
# Although first 4 components descrive only 42.4% of the information in the data set, after 4 components there is a shape decline in the 
# percentage of variation being explained.
# The cell types are not easily separated.
# While there are some cells in the data that are not completely within the data mainstream, there are no blatant outliers.



# ------------------------------------------------------------------------------
# A plot of the loadings of the 1st three principal components for the cell segmentation data
# ------------------------------------------------------------------------------
segRot <- as.data.frame(segPCA$rotation[, 1:3])



# ----------
# Derive the channel variable
vars <- rownames(segPCA$rotation)
channel <- rep(NA, length(vars))
channel[grepl("Ch1$", vars)] <- "Channel 1"
channel[grepl("Ch2$", vars)] <- "Channel 2"
channel[grepl("Ch3$", vars)] <- "Channel 3"
channel[grepl("Ch4$", vars)] <- "Channel 4"

segRot$Channel <- channel
segRot <- segRot[complete.cases(segRot),]
segRot$Channel <- factor(as.character(segRot$Channel))



# ----------
# Plot a scatterplot matrix of the first three rotation variables
# channel 1:  associated with the cell body
# channel 2:  associated with the ecell nucleus
# channel 3:  associated with actin
# channel 4:  associated with tublin

transparentTheme(pchSize = .8, trans = .7)
panelRange <- extendrange(segRot[, 1:3])

library(ellipse)

upperp <- function(...)
{
  args <- list(...)
  circ1 <- ellipse(diag(rep(1, 2)), t = .1)
  panel.xyplot(circ1[,1], circ1[,2],
               type = "l",
               lty = trellis.par.get("reference.line")$lty,
               col = trellis.par.get("reference.line")$col,
               lwd = trellis.par.get("reference.line")$lwd)
  circ2 <- ellipse(diag(rep(1, 2)), t = .2)
  panel.xyplot(circ2[,1], circ2[,2],
               type = "l",
               lty = trellis.par.get("reference.line")$lty,
               col = trellis.par.get("reference.line")$col,
               lwd = trellis.par.get("reference.line")$lwd)
  circ3 <- ellipse(diag(rep(1, 2)), t = .3)
  panel.xyplot(circ3[,1], circ3[,2],
               type = "l",
               lty = trellis.par.get("reference.line")$lty,
               col = trellis.par.get("reference.line")$col,
               lwd = trellis.par.get("reference.line")$lwd)
  panel.xyplot(args$x, args$y, groups = args$groups, subscripts = args$subscripts)
}


splom(~segRot[, 1:3],
      groups = segRot$Channel,
      lower.panel = function(...){}, upper.panel = upperp,
      prepanel.limits = function(x) panelRange,
      auto.key = list(columns = 2))



# -->
# For the first principal component, the loadings for the 1st channel (associated with the cell body) are on the extremes.
# This indicates that cell body characteristics have the largest effect on the 1st principal component and by extension the predictor values.

# Also note that the majority of the loadings for the third channel (measuring actin and tubulin) are closer to zero for the 1st component.
# Conversely, the third principal component is mostly associated with the 3rd channel while the cell body channel plays a minor role here.

# Even though the cell body measurements account for more variation in the data,
# this does not imply that these variables will be associated with predicting the segmentation quality.

