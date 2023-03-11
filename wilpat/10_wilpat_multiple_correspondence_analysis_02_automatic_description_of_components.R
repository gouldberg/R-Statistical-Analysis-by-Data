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
# Automatic description of the component
# ------------------------------------------------------------------------------

dimdesc(res.mca)$'Dim 1'$quali




# ----------
dimdesc(res.mca)$'Dim 1'$category


# -->
# Since most variables have two categories, characterisation by category is similar to that calculated from the variables,
# but specifies the direction of the component.



# ----------
# rounding
lapply(dimdesc(res.mca), lapply, round, 4)


lapply(dimdesc(res.mca), lapply, signif, 3)
