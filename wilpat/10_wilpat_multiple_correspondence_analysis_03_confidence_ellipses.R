setwd("//media//kswada//MyFiles//R//wilpat")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  WilPat
# ------------------------------------------------------------------------------

data("WilPat", package = "MPsychoR")

str(WilPat)



# ----------
# Let us pick six items: gay marriage, sexual freedom, gay adoption, gender quotas, affirmative action, and legalized marijuana
# and country variable (Hungary, the USA, and India)
WP6 <- WilPat[, c(32, 38, 41, 44, 45, 46, 47)]



# ----------
# Convert numeric to factor
WP6 <- rapply(WP6, f = as.factor, classes = "numeric", how = "replace")


str(WP6)



# ------------------------------------------------------------------------------
# Multiple Correspondence Analysis by FactoMineR
# ------------------------------------------------------------------------------

library(FactoMineR)

graphics.off()
par(mfrow = c(2,2))


# res.mca <- MCA(WP6)
res.mca <- MCA(WP6, quali.sup = 7)



# ------------------------------------------------------------------------------
# Confidence ellipses around categories of a categorical variable
# (i.e., around the barycentre of the individuals carrying that category)
# to assess whether two categories are significantly different or not
# ------------------------------------------------------------------------------

plotellipses(res.mca, keepvar = c("GayMarriage", "GayAdoption", "SexualFreedom"))




# ------------------------------------------------------------------------------
# Confidence ellipses around categories of a single categorical variable
# ------------------------------------------------------------------------------

# keepvar = 1:  GayMarriage
plotellipses(res.mca, keepvar = 1, label = "none")

