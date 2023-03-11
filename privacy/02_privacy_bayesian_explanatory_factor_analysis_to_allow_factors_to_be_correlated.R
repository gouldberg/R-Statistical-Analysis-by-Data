setwd("//media//kswada//MyFiles//R//privacy")

packages <- c("dplyr", "MPsychoR", "corrplot", "BayesFM")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Privacy
# ------------------------------------------------------------------------------

data("Privacy", package = "MPsychoR")


str(Privacy)



# ----------
Privstd <- scale(Privacy)

head(Privstd)



# ------------------------------------------------------------------------------
# In order to fit a Bayesian Explanatory Factor Analysis (BEFA), we first need to set an upper lmit for the number of factors
# ------------------------------------------------------------------------------

# minimum number of variables per factor
Nid <- 2


# maximum number of factors
# trunc() truncate the values in x toward 0.
( pmax <- trunc(ncol(Privstd) / Nid) )




# ------------------------------------------------------------------------------
# Prior parametrizations for correlation matrix
#   - We first uses an inverse Wishart distribution with parameters v0 (degrees of freedom) and S0 (scale parameter)
#   - v0: it steers the correlation among hte factors, the larger v0, the larger the a priori factor correlation,
#     and, consequently, the larger the number of factors p since factor splitting can occur.
# ------------------------------------------------------------------------------

# We vary v0 systematically from 6 to 15
# We keep S0 at a default value of 1.
# For each paramter setting, the function below simulates 10,000 matrices and computes the maximum absolute correlation
# and teh minimum eigenvalue of each correlation matrix.
set.seed(123)

Rsim <- simul.R.prior(pmax, nu0 = pmax + c(1, 2, 5, 7, 10))


# Prior distribution of correlation matrix of laten factors
summary(Rsim)


plot(Rsim)


# -->
# Left panel: absolute maximum correlation
# Right panel: smallest egenvalue

# We look for a distribution based on v0 which bounds the prior sufficiently away from extreme regions.
# (i.e., regions where we run into identificability issues)
# In our example, a value of v0 = 10 leads to a good prior that avoids extreme cases.

# If we would pick a v0 = 6 or 7, most likely we would run into identifiability issues since these priors allow for multiple highly correlated factors.
# (factor splitting problem).
# Picking values in the area of v0 = 12 - 15 would imply low factor correlations. Consequently, the number of factors tends to be lower.



# ------------------------------------------------------------------------------
# Prior parametrizations for the number of factors
#   - Bayesian Explanatory Factor Analysis (BEFA) uses a Dirichlet prior with concentration parameter k.
# ------------------------------------------------------------------------------

Ksim <- simul.nfac.prior(nvar = ncol(Privstd), Nid = Nid, Kmax = pmax, kappa = c(.1, .2, .5, 1))


summary(Ksim)


# Dirichlet prior parametrizations for number of factors with
# concentration parameter kappa
plot(Ksim)



# -->
# We pick a value of k = 0.2 in order to put more prior weight on a two-factor solution.
# Note that we could also pick k = 0.5 if we want a three-factor solution to be almost equally likely than a two-factor solution.



# ------------------------------------------------------------------------------
# Bayesian Explanatory Factor Analysis (BEFA)
#   - BEFA uses the same basic equations a EFA, and it generates correlated factors.
#     But BEFA uses MCMC to get the posterior distributions of the parameters.
#     Major selling points of this approach are that it allows the factors to be correlated and that it estimates the number of factors,
#     instead of fixing them a priori.
#   - In BEFA, the rotation problem is solved by specifying a dedicated structure for the factor loadings matrix.
#     Factors are eighter associated with least 2-3 measurements, or not associated with any measurements, in which case they are discarded from the model.
# ------------------------------------------------------------------------------

# beta prior with shape parameters kappa0 and xi0:
#  - Default settings of the befa function are kappa0 = xi0 = 1, reflecting a uniform prior.
#  - We set both of them to 0.1, which implies a U-shaped informative prior, that is we believe that tau0 is either close to 0 or 1.

set.seed(222)

fitbefa <- befa(Privstd, Nid = 2, Kmax = pmax, nu0 = 10, kappa = 0.2, kappa0 = 0.1, xi0 = 0.1, burnin = 5000, iter = 50000)



# ----------
# post process the solution in terms of restoring the identification of the model and the consistency of the signs of the factors loadings.
# It is necessary to perform these two operations after fitting the BEFA model; otherwise the solution cannot be interpreted.

# column reordering
fitbefa <- post.column.switch(fitbefa)


# sign switching
fitbefa <- post.sign.switch(fitbefa)



# ----------
summary(fitbefa)


# -->
# The number of factors proposed by the BEFA is 3 factors, and a posterior probability of 100%, strong evidence that a 3-factor solution fits best.



# ----------
plot(fitbefa)


# -->
# heatmaps for the loadings (those with highest posterior probabilities) and the factor correlations
# For the apc variables, we get two factors, moderately correlated in positive direction.
# The dpc variables load on a third factor, which has a smaller negative correlation with the remaining two factors.
