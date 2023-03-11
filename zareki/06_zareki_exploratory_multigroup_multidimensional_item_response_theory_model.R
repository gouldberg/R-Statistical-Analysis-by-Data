setwd("//media//kswada//MyFiles//R//zareki")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# using all binary ZAREKI addition and subtraction items
itzareki <- zareki[, 1:16]



# ------------------------------------------------------------------------------
# Exploratory multigroup multidimensional IRT
#   - In this example, it is difficult to interpret the two dimensions without considering additional specific characteristics of the items itself
#     since the dimensions are certainly not addition and subtraction.
#     Exploratory multigroup MIRT i nothing else than testing for DIF (Differential Item Functioning)
# ------------------------------------------------------------------------------

class2 <- zareki$class

levels(class2) <- c("second", "thirdfourth", "thirdfourth")



# ----------
# We group the class variable into two categories and fit a multigroup 2D-2PL which results in separate sets of parameter estimates for both groups.
# We are interested in testing whether the discrimination parameters (on the first dimension) and the intercepts differ across the two groups.
modMG <- multipleGroup(itzareki, model = 2, group = class2, SE = TRUE)

summary(modMG)



# ----------
# For testing parameter equality across the groups, we apply Wald test and correct the p-values by means of the false discovery rate
astiDIF <- DIF(modMG, c("a1", "d"), Wald = TRUE, p.adjust = "fdr")

round(astiDIF$adj_pvals[astiDIF$adj_pvals < 0.05], 4)



# -->
# p-valus for the items flagged for DIF (the corresponding p-values are significant)
# Note that strictly speaking, these results reflect a combination of DIF and latent trait distribution effects.
# The baseline model should contain a set of anchor items such that we properly equate the groups.




