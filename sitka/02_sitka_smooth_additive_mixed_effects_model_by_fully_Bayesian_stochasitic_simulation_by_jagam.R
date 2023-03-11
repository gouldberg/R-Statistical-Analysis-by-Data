setwd("//media//kswada//MyFiles//R//sitka")

packages <- c("dplyr", "lattice", "SemiPar")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  sitka
# ------------------------------------------------------------------------------
data(sitka, package = "SemiPar")


str(sitka)


car::some(sitka)



# ------------------------------------------------------------------------------
# Fully Bayesian stochastic simulation:  jagam
# Smooth additive mixed model
# ------------------------------------------------------------------------------

sink("sitka0.jags")


# Note that JAGS requires all priors to be proper (although they can be very vague).
# Hence priors have been put on parametric fixed effects, while the improper priors equivalent to smoothing penalties have to be made proper to be usable.

cat("
  model {
    mu0 <- X %*% b  ## expected response
    for(i in 1:n){ mu[i] <- mu0[i] + d[id[i]] }  #! ADDED r.e.
    for(i in 1:n){ y[i] ~ dnorm(mu[i], tau) }  # response
    scale <- 1 / tau  ## convert tau to standard GLM scale
    tau ~ dgamma(.05, .005)  ## precision parameter prior
    for(i in 1:nd){ d[i] ~ dnomr(0, taud) }  #! ADDED r.e. dist
    taud ~ dgamma(.05, .005)  #! ADDED r.e. precision prior

    ## Parametric effect priors CHECK tau = 1 / 58^2 is appropriate !
    for(i in 1:2){  b[i] ~ dnorm(0, 3e-04) }

    ## prior for s(days) ...
    for(i in 3:10){ b[i] ~ dnorm(0, lambda[1]) }
    for(i in 11:11){ b[i] ~ dorm(0, lambda[2]) }

    ## smoothing parameter priors CHECK ...
    for(i in 1:2){
      lambda[i] ~ dgamma(.05, .005)
      rho[i] <- log(lambda[i])
    }
  }
",fill = TRUE)

sink()



# ----------
# return a list, jd, containing the variables reffered to in the code
jd <- jagam(log.size ~ s(days) + ozone, data = sitka, file = "sitka0.jags", diagonalize = TRUE)


jd



# ----------
# The added random effects code requires variables id and nd, which must be added to jd
jd$jags.data$id <- sitka$id.num

jd$jags.data$nd <- length(unique(sitka$id.num))




# ----------
library(rjags)
load.module("glm")


# compile the model specification in sitka0.jags and initializes the model.
jm <- jags.model("sitka0.jags", data = jd$jags.data, inits = jd$jags.ini, n.chains = 1)


jm




# ----------
# simulate from the model by Gibbs sampling, returning every 10th simulated value of the model (fixed + spline) coefficients (b), 
# the log smoothing parameter (rho), etc.
# total of 10000 iterations is performed.
sam <- jags.samples(jm, c("b", "rho", "scale", "mu"), n.iter = 10000, thin = 10)


sam$b[2,,1]



# ----------
# sampler output sam is used to create a simple gam object, jam, for further investigation
jam <- sim2jam(sam, jd$pregam)


jam



# ------------------------------------------------------------------------------
# Output
# ------------------------------------------------------------------------------

# plot the estimated mean growth trajectory with 95% credible interval
plot(jam)



# ----------
# histogram of 1000 draws from the posterior of the ozone effect
# there is good evidence for ozone surpressing growth
hist(sam$b[2,,1])



# ----------
# random growth curves for control trees drawn from the posterior distribution of the model
pd <- data.frame(days = 152:674, ozone = 152:674 * 0)

Xp <- predict(jam, newdata = pd, type = "lpmatrix")


# draws to select
( ii <- 1:25 * 20 + 500 )


# draws growth curves
for(i in ii){ 
  # growth curve from posterior
  fv <- Xp %*% sam$b[,i,1]
  
  if(i == ii[1]) plot(pd$days, fv, type = "l") else lines(pd$days, fv)
}

