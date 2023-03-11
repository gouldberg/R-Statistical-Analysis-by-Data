setwd("//media//kswada//MyFiles//R//api")

packages <- c("dplyr", "survey")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  subsample from the California Academic Performance Index
# ------------------------------------------------------------------------------

data(api)

str(apiclus1)



# ------------------------------------------------------------------------------
# Create replicate weights
# ------------------------------------------------------------------------------

# the one-stage cluster sample from the California Academic Performance Index 
dclus1 <- svydesign(id = ~ dnum, weights = ~ pw, data = apiclus1, fpc = ~ fpc)

summary(dclus1)



# ----------
# Converting this design to use unstratified jackknife (JK1) weights is as simple as

rclus1 <- as.svrepdesign(dclus1)

summary(rclus1)



# ----------
# To convert to bootstrap weights, specify the type and the number of bootstrap replicates.

bclus1 <- as.svrepdesign(dclus1, type = "bootstrap", replicates = 100)

summary(bclus1)



# ----------
# Balanced Repeated Replicates are designed for surveys with two PSUs in each stratum
# (although as.svrepdesign will attempt to handle some other designs. Within each replicate each stratum of the sample is split into halves,
# in a complex design that ensures two PSUs from different strata are in the same half-sample for exactly half the replicates.
                                                                                   
