setwd("//media//kswada//MyFiles//R//brainIQ")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brainIQ
#   - A simple dataset from Willerman et al. (1991), involving verval IQ(VIQ), performance IQ(PIQ), and brain size (measured in MRI pixel counts)
#   - Authors collected a sample of 40 psychology students. The students took subtests of the Wechsler Adult Intelligence Scale-Revised (WAIS-R)
#     which resulted in a full scale IQ (FSIQ), a verbal IQ (VIQ), and a performance IQ (PIQ).
#     In addition to gender, body height, and body weight, the dataset includes an MRI pixel count vairable which measures the brain size.
# ------------------------------------------------------------------------------

data("BrainIQ", package = "MPsychoR")

str(BrainIQ)


# ----------
X <- BrainIQ[, c("VIQ", "PIQ", "MRI_Count")]

head(X, 4)


# -->
# In this dataset the variables are measured on different units:
# the brain size magnitude is in the 1M area, whereas the VIQ and PIQ are on the usual IQ scale.



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------

psych::describe(X)


hist(X[,"MRI_Count"],  breaks = seq(750000, 1100000, 10000), freq=FALSE)
lines(density(X[,"MRI_Count"]), col = "red", lwd = 2)
rug(X[,"MRI_Count"])



