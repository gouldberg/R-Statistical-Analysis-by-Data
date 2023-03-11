setwd("//media//kswada//MyFiles//R//cell_segmentation")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cell Segmentation in High-Content Screening
#   - Medical researchers oftern seek to understand the effects of medicines or diseases on the size, shape, development status,
#     and number of cells in a living organism or plant.  To do this, experts can examine the target serum or tissue under a microscope and
#     manually asess the desired cell characteristics. This work is tedious and requires expert knowledge of the cell type and characteristics.
#   - Another way to measure the cell characteristics from these kinds of samples is by using high-content screening (Giuliano et. al. 1997)
#     Briefly, a sample is first dyed with a substance that will bind to the desired characteristic of the cells.
#   - But, using an automated, high-throughput approach to assess samples' cell characteristics can sometimes produce misleading results.
#     Hill et al. (2007) descrive a research project that used high-content screening to measure several aspects of cells.  They obserbed that 
#     the imaging software used to determine the location and shape of the cell had difficulty segmenting cells (i.e., defining cells' boundaries).
#   - For this research, Hill et al. (2007) assembled a data set consisting of 2,019 cells. Of these cells, 1,300 were judged to be poorly segmented (PS)
#     and 719 were well segmented (WS); 1,009 cells were reserved for the training set.
#     For all cells, 116 features (e.g., cell area, spot fiber count) were measured and were used to predict the segmentation quality of cells.
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


