setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# We consider eight binary subtraction items only
zarsub <- zareki[, grep("subtr", colnames(zareki))]

str(zarsub)



# ----------
# compute tetrachoric correlation matrix

library(psych)

tetcor <- tetrachoric(zarsub)

tetcor

tetcor$rho




# ------------------------------------------------------------------------------
# Remove subtr5 and refit the Rasch model
#   - In practice, it is suggested to run these tests on multiple external binary covariates.
# ------------------------------------------------------------------------------

fitrasch2 <- RM(zarsub[, -5])

LRtest(fitrasch2, timecat)



# -->
# The nonsignificant p-value suggest that the model fits.


Waldtest(fitrasch2, timecat)




# ------------------------------------------------------------------------------
# esimated parameters
# ------------------------------------------------------------------------------

# eta (item difficulty parameter)
fitrasch2$etapar


# beta (individual item location parameter)
fitrasch2$betapar


# difficulty parameters sorted from the easiest to the most difficult item
round(sort(-fitrasch2$betapar), 3)


