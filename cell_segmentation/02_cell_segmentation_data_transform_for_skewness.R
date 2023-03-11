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
# basics:  check skewness
#
# The column VarIntenCh3 measures the standard deviation of the intensity of the pixels in the actin filaments
# ------------------------------------------------------------------------------
histogram(~ segTrainX$VarIntenCh3, xlab = "Natural Units", type = "count")


max(segTrainX$VarIntenCh3)/min(segTrainX$VarIntenCh3)


library(e1071)
skewness(segTrainX$VarIntenCh3)



# --> 
# the skewness statistic was calculated to be 2.39 while the ratio to the largest and smallest value was 870
# A general rule of thumb to consider is that skewed data whose ratio of the highest value to the lowest value is greater than 20
# have significant skewness



# ------------------------------------------------------------------------------
# transform for skewness for single predictor by Box-Cox transformation
# ------------------------------------------------------------------------------
segPP <- preProcess(segTrainX, method = "BoxCox")

segPP



# --> 
# For the segmentation data, 69 out of 116 predictors were not transformed due to zero or naegative values
# and 3 predictors had lambda estimates within 1 +- 0.02, so no transformation was applied.
# The remaining 44 predictors had values estimated between -2 and 2.



# Apply the transformations
segTrainTrans <- predict(segPP, segTrainX)

segTrainTrans



# ----------
# Results for a single predictor
segPP$bc$VarIntenCh3


# --> estimated lambda is 0.1, indicating the log transformation is reasonable


histogram(~ segTrainX$VarIntenCh3, xlab = "Natural Units", type = "count")


histogram(~ log(segTrainX$VarIntenCh3), xlab = "Log Units", ylab = " ", type = "count")



# ----------
segPP$bc$PerimCh1


histogram(~ segTrainX$PerimCh1, xlab = "Natural Units", type = "count")

histogram(~segTrainTrans$PerimCh1, xlab = "Transformed Data", ylab = " ", type = "count")

