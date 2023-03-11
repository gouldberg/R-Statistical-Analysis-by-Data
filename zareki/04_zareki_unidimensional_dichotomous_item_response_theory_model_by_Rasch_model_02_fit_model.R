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
# Unidimensional Dichotomous IRT Models:  The Rasch Model
#   - The Rasch model can be used if we aim to construc a scale that fulfills highest measurement standards.
#   - The Rasch model is formally defined as:  P(X(vi) = 1) = exp(theta(v) + beta(i)) / { 1 + exp(theta(v) + beta(i)) }
#     modeling probability that person v scores 1 on item i
#     beta(i): individual item location parameter, often referred to as item easiness parameter, - beta(i) is the item difficulty parameter
#     theta(v): person ability parameter
#     Both beta(i) and theta(v) are on an interval scale and can be mapped on the same latent trait (they are directly comparable)
#
#  -  The Rasch model has three fundamental assumptions:
#       - unidimensionality of the latent trait
#       - parallel item characteristic curves (ICCs)
#       - local independence:  given a person paramter, item responses become independent. (this assumption is difficult to check and often omitted in practice)
#
#  -  Rasch models are nothing else than mixed-effects logistic regressions with items as fixed effects and persons as random effects.
#     The random intercepts correspond to the person parameters, the fixed effect parameters to the item location parameters.
# ------------------------------------------------------------------------------

library(eRm)


# eRm package uses a conditional maximum likelihood (CML) approach, which has some advantages over other IRT estimation approaches
fitrasch1 <- RM(zarsub)

fitrasch1



# ----------
# eta (item difficulty parameter)
fitrasch1$etapar


# beta (individual item location parameter)
fitrasch1$betapar


# beta = W (design matrix) * (- eta)
as.vector(fitrasch1$W %*% - fitrasch1$etapar)


# difficulty parameters sorted from the easiest to the most difficult item
round(sort(-fitrasch1$betapar), 3)



