setwd("//media//kswada//MyFiles//R//chimpanzees")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prosocial chimpanzees
# ------------------------------------------------------------------------------
data("chimpanzees", package = "rethinking")

d <- chimpanzees

dim(d)

str(d)


# ----------
# clean NAs from the data
d2 <- d
d2$recipient <- NULL

d$block_id <- d$block

unique(d$actor)



# ------------------------------------------------------------------------------
# model with varying slopes:  centered parametrization
# ------------------------------------------------------------------------------

mod7 <- map2stan(
  alist(

    # likelihood
    pulled_left ~ dbinom(1, p),

    # linear model
    logit(p) <- A + (BP + BPC * condition) * prosoc_left,
    A <- a + a_actor[actor] + a_block[block_id],
    BP <- bp + bp_actor[actor] + bp_block[block_id],
    BPC <- bpc + bpc_actor[actor] + bpc_block[block_id],
    
    # adaptive priors
    c(a_actor, bp_actor, bpc_actor)[actor] ~ dmvnorm2(0, sigma_actor, Rho_actor),
    c(a_block, bp_block, bpc_block)[block_id] ~ dmvnorm2(0, sigma_block, Rho_block),

    # fixed priors
    c(a, bp, bpc) ~ dnorm(0, 1),
    sigma_actor ~ dcauchy(0, 2), 
    sigma_block ~ dcauchy(0, 2), 
    Rho_actor ~ dlkjcorr(4),
    Rho_block ~ dlkjcorr(4)
  ),
  data = d, chains = 3, iter = 5e3, warmup = 1e3, cores = 10
)



plot(mod7)

precis(mod7, digits=3, depth = 2)



# ------------------------------------------------------------------------------
# model with varying slopes:  non-centered parametrization
# ------------------------------------------------------------------------------
# apply NON-CENTERED parametrization to adaptive priors by dmvnormNC()

mod7.NC <- map2stan(
  alist(
    
    # likelihood
    pulled_left ~ dbinom(1, p),
    
    # linear model
    logit(p) <- A + (BP + BPC * condition) * prosoc_left,
    A <- a + a_actor[actor] + a_block[block_id],
    BP <- bp + bp_actor[actor] + bp_block[block_id],
    BPC <- bpc + bpc_actor[actor] + bpc_block[block_id],
    
    # adaptive NON-CENTERED priors
    c(a_actor, bp_actor, bpc_actor)[actor] ~ dmvnormNC(sigma_actor, Rho_actor),
    c(a_block, bp_block, bpc_block)[block_id] ~ dmvnormNC(sigma_block, Rho_block),
    
    # fixed priors
    c(a, bp, bpc) ~ dnorm(0, 1),
    sigma_actor ~ dcauchy(0, 2), 
    sigma_block ~ dcauchy(0, 2), 
    Rho_actor ~ dlkjcorr(4),
    Rho_block ~ dlkjcorr(4)
  ),
  data = d, chains = 3, iter = 5e3, warmup = 1e3, cores = 10
)



plot(mod7.NC)

precis(mod7, digits=3, depth = 2)



# ----------
# compare effective number of samples
neff_c <- precis(mod7, 2)@output$n_eff
neff_nc <- precis(mod7.NC, 2)@output$n_eff

par(mfrow=c(1,1))
boxplot(list('mod7' = neff_c, 'mod7.NC' = neff_nc), ylab = "effective samples", xlab = "model")


# -->
# Non-centered parametrizaion model produces no divergent iterations.
# It also sampled noticeably faster than centered-parametrization model
# Also the effective number of samples is much larger.



# ----------
# This model has 54 parameters: 3 average effects, 3 * 7 varying effects on actor, 3 * 6 varying effects on block, 6 standard deviations, and 6 free correlation parameters. 
# But effectively the model has only 18 parameters.
# So as usual, each varying intercept or slope counts less than one effective parameter.
WAIC(mod7.NC)



# ------------------------------------------------------------------------------
# model inspection
#   - posterior standard deviation of parameters -- how aggressively the varying effects are being regularized
# ------------------------------------------------------------------------------
precis(mod7.NC, depth = 2, pars = c("sigma_actor", "sigma_block"))


# -->
# The [1]: varying intercept standard deviation
# The [2] and [3]: for slopes

# You can get a sense from the small values that shrinkage is pretty aggressive here.
# This is what takes the model from 56 actual parameters to 18 effective parameters, as measured by WAIC



# ------------------------------------------------------------------------------
# for remarks
# ------------------------------------------------------------------------------
# linkk will return a list of matrices, one matrix for each linear model
p <- link(mod7.NC)
str(p)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
compare(mod6, mod7.NC)

precis(mod6, pars = c("sigma_actor", "sigma_block"))
precis(mod7.NC, depth = 2, pars = c("sigma_actor", "sigma_block"))


# -->
# Hardly any predictive difference between models
# The varying slopes model (mod7.NC) makes very similar predictions, because there is not much variation across actors or block in the slopes.


# In this example, no matter which varying effect structure you use, you'll find that actors vary a lot in their baseline preference for the left-hand lever.
# Everything else is much less important.

# But using MOST COMPLEX model mod7.NC tells the correct story.
# Because the varying slopes are adaptively regularized, the model has not overfit much, relative to the simpler model that contains only the important intercept variation.



