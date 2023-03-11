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
# Item response functions (Item Characteristics Curves)
# ------------------------------------------------------------------------------

# item response functions (ICCs)
plotjointICC(fitrasch2, xlab = "Subtraction Trait", main = "ICCs Subtraction Items")



# -->
# We see that all ICCs are parallel, and all of them have a slope of 1.
# The plot show that item1 is the easiest item and item3 is the most difficult one.

# In this plot, the item parameters are reflected the value on the x-axis for which the endorsement probability is exactly 0.5.



