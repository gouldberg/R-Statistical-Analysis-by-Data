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
# Logistic Regression DIF (Differntial Item Functioning) detection
#   - The idea of this approach is to specify a set of logistic regression equations and predict the original item responses
#     from the person parameters theta and the external grouping variable z.
#     This method works for dichotomous as well as polytomous responses and allows for a single grouping variable with multiple categories.
#   - The following set of proportional odds models is formulated
#       - M1:  logit(P(xi)) = tau0 + tau1 * theta
#       - M2:  logit(P(xi)) = tau0 + tau1 * theta + tau2 * z
#       - M3:  logit(P(xi)) = tau0 + tau1 * theta + tau2 * z + tau3 * theta * z
#     We are interested in comparing the following models via the LR-principle:
#       - M2 vs. M1:  if significant, we have uniform DIF (the ICCs are shifted in location across subgroups, but they remain parallel, a group main effect)
#       - M3 vs. M2:  if significant, we have nonuniform DIF (the ICCs across subgroups are shifted, and they cross (an interaction effect between group and the trait)
#
#   - lordif package fits a GRM (Graded Response Model) or a GPCM (Generalized Partial Credit Model) using mirt package,
#     uses Stocking-Lord equating for the item parameters, and estimates person parameters based on DIF and non-DIF items.
# ------------------------------------------------------------------------------

# Using all the CDI items of the yourth depression dataset
cdi <- YouthDep[,1:26]

table(YouthDep$race)



# ----------
# The authors were interested in DIF analyses on an external race variable (four categories).
# Note that the aim was not to eliminate items from the CDI, which is a well-established scale.
# Rather, the authors wanted to identify DIF items and score all individuals in a "fair" way by means of group-specific person parameter estimates for items flagged as DIF.

library(lordif)


# lordif() performs logistic ordinal regression differential item runctioning using IRT
cdiDIF <- lordif(cdi, YouthDep$race, criterion = "Chisqr")

summary(cdiDIF)

sum(cdiDIF$flag)



# -->
# In total, 20 out of 26 items are flagged as DIF.
# 2 categories were collapsed due to an insufficient number of responses within each subgroup



# ----------
# p-values of LR-tests for the first 3 items
head(cdiDIF$stats)

cdiDIF$stats[1:3, 1:5]



# -->
# We see that for the first item, none of the LR-X^2 values is significant, in fact, item 1 was not flagged.
# For the second item, X12^2 (M2 vs M1) is significant, whereas X23^2 (M3 vs M2) is not significant.
# Thus the second item has uniform DIF.
# For the third item, all p-values are significant, the case of nonuniform DIF.



# ----------
# plots including item true score functions (the Graded Response Model probabilities related to the original 0-2 scale)
# Each panel contains information on the p-values of the corresponding LR-tests according to eh models as well as differences in McFadden's R^2
# McFadden's R^2:  < 0.13 is the effect size is negligible, 0.13 - 0.26 moderate, > 0.26 large.s
plot(cdiDIF, labels = c("white", "Black", "Asian", "Latino"))



# -->
# We see that CDI2r has uniform DIF, whereas CDI3 has nonuniform DIF.



# ----------
# Graded Response Model parameters (discrimination, cateogory boundaries) for the six non-DIF items and the first DIF item (I2)
head(cdiDIF$ipar.sparse, 10)



# -->
# Note that for some items, there is only one category boundary.
# This results from the fact that there were not enough observations in a particular category for parameter estimation.
# For such cases, lordif collapses categories automatically.

# Item 2 was flagged as DIF. We get 4 sets of discrimination / boundary parameters, one of each race category.



# ----------
# The calibrated, group-specific person parameter vector
# Based on the DIF subgroup structure, they are fairly scored, are on the same scale, and can be subject to further analysis.
( ppar <- cdiDIF$calib.sparse$theta )





