setwd("//media//kswada//MyFiles//R//els_career_academy")

packages <- c("dplyr", "mice", "mitools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Estimate propensity scores using classification trees
#   - The use of cassification and regression trees for propensity score estimation is not recommended because the results have a high level
#     of variability with many covariates and frequently produce poor estimates of propensity scores.
#     Although classification trees are capable of detecting complex interactions between covariates, they do not perform well if the funcitonal form
#     of the relationship between covariates and outcome is linear and addtive.
#   - Also one limitation of recursive partitioning algorithms is that they tend not to perform well when the distribution of the outcome
#     is strongly skewed. For propensity score estimation, this problem could manifest if the proportion of treated individuals is small.
# ------------------------------------------------------------------------------

library(party)



psFormula


# fit the tree to imputed data
myctree <- ctree(psFormula, data = imputation1)


myctree




# ----------
# obtain tiff image
tiff("ctree_for_treat.tif", res=600, compression = "lzw", height=6, width=15, units="in")

plot(myctree)

dev.off()


# obtain a pdf
# pdf("ctree_for_treat.pdf", height=6, width=15)
# plot(myctree)
# dev.off()


