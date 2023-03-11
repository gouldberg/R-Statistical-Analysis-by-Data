# setwd("//media//kswada//MyFiles//R//mite")
setwd("//media//kswada//MyFiles//R//Spatial_data_analysis//mite")

packages <- c("dplyr", "ape", "spdep", "ade4", "adegraphics", "adespatial", "vegan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mite
#  - Substrate (7 classes),  Shrubs (3 classes), and Microtopography (2 classes)
# ------------------------------------------------------------------------------

load("./data/mite.RData")


dim(mite)
car::some(mite)


dim(mite.env)
car::some(mite.env)


dim(mite.xy)
car::some(mite.xy)



source("./functions/plot.links.R")
source("./functions/sr.value.R")
source("./functions/quickMEM.R")
source("./functions/scalog.R")



# ------------------------------------------------------------------------------
# regression of the significant canonical axes on the environmental variables with Shapiro-Wilk normality tests of residuals
# canonical axes 1
# ------------------------------------------------------------------------------

mite.rda2.axis1.env <- lm(mite.rda2.axes[ ,1] ~ ., data = mite.env)


summary(mite.rda2.axis1.env)



# -->
# significant term: SubsDens, ShrubMany



# ----------
car::residualPlots(mite.rda2.axis1.env)



# ----------
shapiro.test(resid(mite.rda2.axis1.env))



# -->
# note rejecting --> normality



# ------------------------------------------------------------------------------
# regression of the significant canonical axes on the environmental variables with Shapiro-Wilk normality tests of residuals
# canonical axes 2
# ------------------------------------------------------------------------------

mite.rda2.axis2.env <- lm(mite.rda2.axes[ ,2] ~ ., data = mite.env)


summary(mite.rda2.axis2.env)



# -->
# significant term: ShrubNone, TopoHummock



# ----------
car::residualPlots(mite.rda2.axis2.env)



# ----------
shapiro.test(resid(mite.rda2.axis2.env))



# -->
# note rejecting --> normality




# ------------------------------------------------------------------------------
# regression of the significant canonical axes on the environmental variables with Shapiro-Wilk normality tests of residuals
# canonical axes 3
# ------------------------------------------------------------------------------

mite.rda2.axis3.env <- lm(mite.rda2.axes[ ,3] ~ ., data = mite.env)


summary(mite.rda2.axis3.env)



# -->
# significant term: SubstrateSphagn1, ShrubMany, ShrubNone



# ----------
car::residualPlots(mite.rda2.axis3.env)



# ----------
shapiro.test(resid(mite.rda2.axis3.env))



# -->
# note rejecting --> normality


# Depending on the permutation-based p-value the third axis may not be significant.



# -->
# The 3 spatial axes are not related to the same environmental variables (excpt for shrubs).
# They are not fully explained bby them either.
# A precise assessment of the portions explained will require variation partitioning.


# This dbMEM analysis produced spatial models combining all the spatial variables forward-selected from the set of 22 dbMEM with
# positive spatial correlation.
# Here the 3 significant canonical axes are a combination of dbMEM variables ranging from broad (dbMEM1) to fine scale (dbMEM20).

# If one is interested in the global spatial structure of the response data,
# it does not allow one to discriminate between broad, medium and fine-scaled structures
# since all significant dbMEM are combined.

