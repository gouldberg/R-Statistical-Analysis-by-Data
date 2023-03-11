setwd("//media//kswada//MyFiles//R//rugged")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rugged
# ------------------------------------------------------------------------------
data("rugged", package = "rethinking")

d <- rugged

dim(d)

str(d)



# ----------
# make log version of outcome
d$log_gdp <- log(d$rgdppc_2000)


# extract countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000), ]



# ------------------------------------------------------------------------------
# regression modeling with interactions by map():  quadratic approximation
# ------------------------------------------------------------------------------

# This model aims to predict log-GDP with terrain ruggedness, continent, and the interaction of the two.
mod1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged + bA * cont_africa + bAR * rugged * cont_africa,
    a ~ dnorm(0, 100),
    bA ~ dnorm(0, 10),
    bR ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), 
  data = dd
)



# ------------------------------------------------------------------------------
# regression modeling with interactions by map2stan() to use Hamiltonian Monte Carlo
# ------------------------------------------------------------------------------
# preparation
#  - Preprocess all variable transformations
#  - Make a new trimmed down data frame that contains only the variables you will actually use to fit the model
#    Technically, you do not have to do this. But doing so avoids common problems. For example, if any of the unused variables have missing values, NA,
#    then Stan will refuse to work.

dd.trim <- dd[, c("log_gdp", "rugged", "cont_africa")]

str(dd.trim)



# ----------
# Estimation
# The uniform prior on sigma has been changed to a half-CAUCHY prior.
# This is a weakly regularizing prior for standar deviations
mod1.stan <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR * rugged + bA * cont_africa + bAR * rugged * cont_africa,
    a ~ dnorm(0, 100),
    bA ~ dnorm(0, 10),
    bR ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 2)
  ), 
  data = dd.trim
)



# check raw Stan code
stancode(mod1.stan)



# ----------
# Checking the chain by trace plot
# The gray region in each plot, the first 1000 samples, marks the adaptation samples.
# During adaptation, the Markov chain is learning to more efficiently sample from the posterior distribution.
# They are automatically discarded by extract.samples.

plot(mod1.stan)




# ----------
# These estimates are very similar to the quadratic approximation
# But note that the interval boundaries in the table are highest posterior density intervals (HPDI), not ordinary percentile intervals (PI)
precis(mod1.stan, digits = 3)

precis(mod1, digits = 3)



