setwd("//media//kswada//MyFiles//R//rwdq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RWDQ (Work Design Questionnaire R Package Authors)
# ------------------------------------------------------------------------------

data("RWDQ", package = "MPsychoR")

str(RWDQ)


car::some(RWDQ)




# ------------------------------------------------------------------------------
# Intercept-slope parameterization
#   - It has been found that there is a strong parametric relationship between factor analysis and IRT.
#     logit(P(X(vi))) = alpha(i) * (theta(v) - beta(i))
#     --> reparametrized logit(P(X(vi))) = alpha(i) * theta(v) + d(i),   d(i) = - beta(i) * alpha(i)
#     Within an IRT context, we call theta(v) "person parameters" whereas within a factor analysis context "factor scores"
#
#   - All unidimensional IRT models can be reformulated using the intercept-slope parameterization.
# ------------------------------------------------------------------------------

library(ltm)


irtpar <- ltm(RWDQ1 ~ z1)

fapar <- ltm(RWDQ1 ~ z1, IRT.param = FALSE)



# ----------
round(head(cbind(coef(irtpar), coef(fapar))), 3)



# ----------
# Compute the person parameters
irtppar <- factor.scores(irtpar)$score.dat$z1

fappar <- factor.scores(fapar)$score.dat$z1

identical(irtppar, fappar)



