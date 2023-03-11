setwd("//media//kswada//MyFiles//R//trolley")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Trolley
# ------------------------------------------------------------------------------
data("Trolley", package = "rethinking")

d <- Trolley

dim(d)

str(d)



# ------------------------------------------------------------------------------
# estimate cumulative probabilities for each response
# ------------------------------------------------------------------------------
# phi: a placeholder for the linear model that incorporates predictor variables
# a constant zero for now, because only the intercept parameters are of interest

# start values for the intercepts are chosen just to start them in the right order.
# exact values are not important, but their ordering, on the log-cumulative-odds scale, is important.

mod1 <- map(
  alist(
    response ~ dordlogit(phi, c(a1,a2,a3,a4,a5,a6)),
    phi <- 0,
    c(a1, a2, a3, a4, a5, a6) ~ dnorm(0,10)
  ),
  data = d,
  start = list(a1=-2, a2=-1, a3=0, a4=1, a5=2, a6=2.5)
)



# ----------
# posterior distribution on the log-cumulative-odds scale
precis(mod1, corr=TRUE, digits=3)



# ----------
# get cumulative probabilities
# This is the same as the values in cum_pr_k 
logistic(coef(mod1))

cum_pr_k



# ------------------------------------------------------------------------------
# estimate cumulative probabilities for each response by Stan's HMC engine
# ------------------------------------------------------------------------------

mod1.stan <- map2stan(
  alist(
    response ~ dordlogit(phi, cutpoints),
    phi <- 0,
    cutpoints ~ dnorm(0,10)
  ),
  data = list(response = d$response),
  start = list(cutpoints = c(-2, -1, 0, 1, 2, 2.5)),
  chains = 2, cores = 10
)


precis(mod1.stan, digits = 3, depth = 2)



