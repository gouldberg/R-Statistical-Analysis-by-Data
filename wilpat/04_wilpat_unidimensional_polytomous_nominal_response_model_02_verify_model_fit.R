setwd("//media//kswada//MyFiles//R//wilpat")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  WilPat
# ------------------------------------------------------------------------------

data("WilPat", package = "MPsychoR")

str(WilPat)



# ----------
wpit15 <- WilPat[,1:15]

names(wpit15)



# ----------
# Eliminate 4 items based on 2-dimensional Princals analysis
elim <- c("Nationalism", "Patriotism", "ChurchAuthority", "Obedience")

ind <- match(elim, colnames(wpit15))

wpitnew <- wpit15[, -ind]




# ------------------------------------------------------------------------------
# Goodness-of-fit assessment
# ------------------------------------------------------------------------------

# M2 statistic proposed by Maydeu-Olivares and Joe (2005).
# It is a limited information version of a X^2-test involving observed probabilities and model probabilities.
# The RMSEA (including 90% CI), TLI, and CFI are borrowed from confirmatory factor analysis (CFA) and structural equation models (SEM), respectively.

M2(nrmwp)



# -->
# Non significant p-value suggests that the model fits.
# CFI = 0.99 > 0.95 is good fit
# For the RMSEA, the CFA/SEM cutoff was 0.05 for a good fitting model, 
# but in IRT, it is suggested to use 0.05 / k (k = number of categories per items) as fit cutoff.
# In our example, k = 3, thus, the RMSEA should be smaller than 0.017, this is the case in our example.


