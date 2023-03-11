setwd("//media//kswada//MyFiles//R//youthdep")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  YouthDep
# ------------------------------------------------------------------------------

data("YouthDep", package = "MPsychoR")

str(YouthDep)

dim(YouthDep)



# ------------------------------------------------------------------------------
# Compute polychoric correlation matrix
# ------------------------------------------------------------------------------

# Using all the CDI items of the yourth depression dataset
DepItems <- YouthDep[,1:26]


# Convert to numeric
Depnum <- data.matrix(DepItems) - 1  



# ----------
# polychoric correation
Rdep <- polychoric(Depnum)

Rdep



# polychoric correation matrix
Rdep$rho



# ------------------------------------------------------------------------------
# Determining the number of factors:  Tucker-Lewis index (TLI)
#   - it compares a worst-case model Q0 (a zero-factor model) and a best-case model with our fitted model Qp
#     TLI(p) = ( Q0 - Qp ) / ( Q0 - 1 )
#   - the larger its value the better the fit.  Hu and Bentler (1999) consider values above 0.90 as "good" fit
# ------------------------------------------------------------------------------
fadep_1 <- fa(Depnum, 1, cor = "poly", fm = "ml")
fadep_2 <- fa(Depnum, 2, cor = "poly", fm = "ml")
fadep_3 <- fa(Depnum, 3, cor = "poly", fm = "ml")



# ----------
summary(fadep_1)

summary(fadep_2)

summary(fadep_3)


# -->
# TLI(1) = 0.76   TLI(2) = 0.823   TLI(3) = 0.876
# We see that the TLI is considerably low, and the RMSEA is barely within an acceptable range.


