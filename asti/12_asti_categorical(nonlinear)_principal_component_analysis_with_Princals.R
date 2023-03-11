setwd("//media//kswada//MyFiles//R//asti")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ASTI
# ------------------------------------------------------------------------------

data("ASTI", package = "MPsychoR")


str(ASTI)



# ----------
st <- ASTI[, c(2, 4, 7, 13, 16, 24, 25)]
pg <- ASTI[, c(11, 14, 15, 17, 18, 23)]


stpg <- data.frame(st = st, pg = pg)
  


# ------------------------------------------------------------------------------
# Princals on Ordinal Data:  Check the loss value to judge whether we should treat this data on a metric scale level or ordinal scale level.
# ------------------------------------------------------------------------------

library("Gifi")


prord <- princals(stpg)

prord


# -->
# By comparing loss value, there is not much of an improvement by fitting a ordinal version.
# We have good evidence that it is fine to treat these data on a metric scale level.



# ----------
plot(prord, plot.type = "screeplot")



# ----------
# Ordinal solution
plot(prord, plot.type = "transplot", var.subset = c(1:2, 8:9), lwd = 2)



# -->
# It shows the transformation plots of 4 selected items.
# We see that ordinality of the original scale (x-axis) is maintained in the transofrmation (y-axis)



# ----------
# Each person gets a score on p = 2 dimensions.
# Object scores are the Princals equivalent to PC scores in PCA
head(round(prord$objectscores, 3), 3)



# ----------
# Each category gets a quantification in the 2D space !!!
# Note that these quantifications on the second dimension are linearly dependent on the ones of the 1st dimension.
# This is a technical feature of Princals, often referred to as rank-1 restriction.
# That is, from a transformation perspective, only the transformations on the 1st dimension matter.
# This is something very convenient because we can use the 1st dimension category quantifications which replace our original data values. --> score matrix
prord$quantifications[1]



# ----------
# original scores and the transformed scores (score matrix) for the 1st 6 observations
head(stpg[, 1:5])

head(round(prord$scoremat[,1:5], 3))



# ----------
# Loading plot
plot(prord, main = "ASTI Loadings plot")


# -->
# We see that, apart from items 24 and 25, the 2nd dimension separates the ST items from the PG items.





