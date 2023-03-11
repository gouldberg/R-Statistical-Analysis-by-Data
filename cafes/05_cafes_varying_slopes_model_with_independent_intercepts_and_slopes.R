setwd("//media//kswada//MyFiles//R//cafes")

packages <- c("dplyr", "rethinking", "MASS", "ellipse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# model treating intercept and slope independently and uses more parameters
# ------------------------------------------------------------------------------

mod2.rho7 <- map2stan(
  alist(
    wait ~ dnorm(mu, sigma),
    mu <- a_cafe[cafe] + b_cafe[cafe] * afternoon,
    # c(a_cafe, b_cafe)[cafe] ~ dmvnorm2(c(a,b), sigma_cafe, Rho),
    a_cafe[cafe] ~ dnorm(a, sigma_a),
    b_cafe[cafe] ~ dnorm(b, sigma_b),
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma_a ~ dcauchy(0, 1),
    sigma_b ~ dcauchy(0, 1),
    sigma ~ dcauchy(0, 1)
    # sigma_cafe ~ dcauchy(0,2),
    # sigma ~ dcauchy(0, 2),
    # Rho ~ dlkjcorr(2)
  ),
  data=data.rho7$data,
  iter=5000, warmup=2000, chains=2, cores = 10
)




# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------

( cmp <- compare(mod.rho7, mod2.rho7) )


plot(cmp)



# First model (with multi-variate Gaussian priors(MVN)) has smaller WAIC and smaller number of effective parameters, thus it is better.
# Smaller number of effective parameters can be explained by the fact that MVN takes into account correlation between slope and intercept 
# and therefore requires fewer parameters to describe data. 
# The second model(mod2.rho7) treats intercept and slope independently and uses more parameters.

