setwd("//media//kswada//MyFiles//R//brains")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brains
# ------------------------------------------------------------------------------

data(brains, package = "gamlss.mx")

str(brains)


car::some(brains)



# ----------
# Since the distribution of both brain size and body weight are highly skewed, a log transformation was applied to both
# variables to give transformed variables

brains <- transform(brains, lbrain = log(brain), lbody= log(body))




# ------------------------------------------------------------------------------
# Fit normal mixture models with 1,2,3 and 4 components
# ------------------------------------------------------------------------------

# A normal error linear regression model of lbrain against lbody has a highly significant slope for lbody but
# it is believed that the data may represent different stages of evolution and so a mixture model is fitted to the data.

# In the mixture model, the evolution stage is represented by a shift in the intercept of the regression equation.
# Normal mixture models with 1, 2, 3 and 4 components are fitted below.

# As the slopes are the same for the K components, parallel lines are fitted
# The plots of the EM trajectories are suppressed here.

br.1 <- gamlss(lbrain ~ lbody, data = brains)


library(gamlss.mx)

# mixture = "np":  nonparametric finite mixtures
br.2 <- gamlssNP(formula = lbrain ~ lbody, mixture = "np", K = 2, tol = 1, data = brains, family = NO, plot.opt = 0)


br.3 <- gamlssNP(formula = lbrain ~ lbody, mixture = "np", K = 3, tol = 1, data = brains, family = NO, plot.opt = 0)


br.4 <- gamlssNP(formula = lbrain ~ lbody, mixture = "np", K = 4, tol = 1, data = brains, family = NO, plot.opt = 0)



# ----------
GAIC(br.1, br.2, br.3, br.4)

GAIC(br.1, br.2, br.3, br.4, k = log(length(brains$body)))



# -->
# The model br.3 with 3 components (i.e. three parallel lines) is selected by both criteria.



# ----------
br.3


# -->
# Model br.3 can be presented as Y ~ NO(mu, sigma) where
# mu = -3.0715 + 0.750 * x:  with probability 0.107
# mu =  1.909 + 0.750 * x:  with probability 0.751
# mu =  3.482 + 0.750 * x:  with probability 0.141

# and sigma = exp(-0.9387) = 0.391


# ----------
# The estimated posterior (conditional) probabilities
head(br.3$post.prob[[1]])



# ----------
# residual plot of the finite mixture model
plot(br.3)
