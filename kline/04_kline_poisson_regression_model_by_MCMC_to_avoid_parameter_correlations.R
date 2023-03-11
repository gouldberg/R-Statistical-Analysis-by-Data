setwd("//media//kswada//MyFiles//R//kline")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Kline
# ------------------------------------------------------------------------------
data("Kline", package = "rethinking")

d <- Kline

dim(d)

str(d)


# ----------
# Theory says that it is the order of magnitude of the population that matters, not the absolute size of it.
d$log_pop <- log(d$population)
d$contact_high <- ifelse(d$contact == "high", 1, 0)



# ------------------------------------------------------------------------------
# poisson regresion by MCMC
# ------------------------------------------------------------------------------
mod1.stan <- map2stan(mod1, iter=3000, warmup = 1000, chains = 4)


precis(mod1, corr=TRUE, digits=3)
precis(mod1.stan, digits=3)

pairs(mod1.stan)


# -->
# Hamiltonian Monte Carlo is very good at handling correlations between parameters, but it would still be better to avoid such strong correlations, when possible.
# The reason is that even HMC is going to be less efficient, when there are strong correlations in the posterior distribution.

# n_eff is low ...


# ------------------------------------------------------------------------------
# Centering predictors reduces correlations among parameters and aid in inference
# ------------------------------------------------------------------------------

# construct centered predictor
d$log_pop_c <- d$log_pop - mean(d$log_pop)


# re-estiamte
mod1.stan.c <- map2stan(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp * log_pop_c + bc * contact_high + bcp * log_pop_c * contact_high,
    a ~ dnorm(0, 10),
    c(bp, bc, bcp) ~ dnorm(0, 1)
  ),
  data = d, iter= 3000, warmup = 1000, chains = 4
)


precis(mod1.stan.c)


# ----------
# centering predictors reduces correlation among parameters !!!
pairs(mod1.stan.c)


# -->
# The Markov chain also was more efficient, resulting in a greater number of effective samples.
