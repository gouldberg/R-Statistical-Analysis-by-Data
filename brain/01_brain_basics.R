setwd("//media//kswada//MyFiles//R//brain")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  brain
#   - The data are from brain iamging by functional magnetic resonance scanning, and were reported in Landau et al. (2003).
#   - Each row of the data frome corresponds to one voxel.
#   - Variables:
#        - X and Y, giving the locations of each voxel;
#        - medFPQ:  the brain activity level measurement (median "Fundamental Power Quotient" over three measurements)
#        - region: a code indicating which region the voxel belongs to (0 for base region; 1 for region of experimental interest and 2 for region subjected to direct stimulation)
#          there are some NA's in this column.
#        - meanTheta: average phase shift at each voxel, which we will not consider further
#   - We consider models for medFPQ as a function of X and Y
# ------------------------------------------------------------------------------
data(brain, package = "gamair")

str(brain)



# 2 voxels appear problematic, these voxels have medFPQ values recorded as 3 * 10^-6 and 4 * 10^-7, while the remaining 1565 voxels have values in the range 0.003 to 20.
# Residual plots from all attempts to model the data set including these two voxels consistently show them as grotesque outliers.

# exclude 2 outliers.
brain <- brain[brain$medFPQ > 5e-3,]



# ------------------------------------------------------------------------------
# basics visualization
# ------------------------------------------------------------------------------

summary(brain$medFPQ)



plot(brain$medFPQ, type="l")



hist(brain$medFPQ, breaks = seq(0, 21, 0.01))

