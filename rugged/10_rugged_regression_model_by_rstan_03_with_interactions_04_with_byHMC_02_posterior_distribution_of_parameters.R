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
# Posterior distribution for each parameters
# ------------------------------------------------------------------------------

pairs(mod1)

pairs(mod1.stan)



# -->
# For this model and these data, the resulting posterior distribution is quite nearly multivariate Gaussian.
# The density for sigma is certainly skewed in the expected direction
# But otherwise the quadratic approximation does almost as well as Hamiltonian Monte Carlo.


# ----------
show(mod1)


show(mod1.stan)




# ------------------------------------------------------------------------------
# Sampling and visualization
# ------------------------------------------------------------------------------

# Sampling:  run 4 independent Markov chains
mod1.stan_4chains <- map2stan(mod1.stan, chains = 4, cores = 10)


precis(mod1.stan_4chains, digits = 3)



# ----------
# by default only 1000 samples for each parameter.
post <- extract.samples(mod1.stan)

str(post)

pairs(post)


