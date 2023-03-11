setwd("//media//kswada//MyFiles//R//troutegg")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  troutegg
# ------------------------------------------------------------------------------

data("troutegg", package = "faraway")

str(troutegg)

head(troutegg)



# ------------------------------------------------------------------------------
# Beta-Binomial GLM
#   - The quasi-binomial method with dispersion parameter is only appropriate when the covariate classes are roughly equal in size.
#     If not, one approach uses the beta-binomial distribution where we assume that p follows a beta distribution.
# ------------------------------------------------------------------------------

library(dispmod)


dmod <- glm.binomial.disp(bmod)



# ----------
sumary(bmod)

# quasi-binomial GLM
sumary(bmod, dispersion = sigma2)

# beta-binomial GLM
sumary(dmod)



# -->
# The results are quite similar to quasi-binomial GLM, because the covariate class sizes do not very much
