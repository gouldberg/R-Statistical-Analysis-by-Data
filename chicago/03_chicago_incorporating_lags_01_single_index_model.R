setwd("//media//kswada//MyFiles//R//chicago")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  chicago
# ------------------------------------------------------------------------------
data(chicago, package = "gamair")

str(chicago)



# ------------------------------------------------------------------------------
# Single index model for pollution related deaths
#   - specified in terms of smooth functions of weighted sums of covariates, where the weights are estimated as part of estimation
#   - In this current case, we would be interested in a weighted sum over lagged covariates.
# ------------------------------------------------------------------------------

# prepare a set of 6-column matrices containing the lagged variables in separate columns
lagard <- function(x, n.lag = 6){
  n <- length(x)
  X <- matrix(NA, n, n.lag)
  for(i in 1:n.lag) X[i:n, i] <- x[1:(n-i+1)]
  X
}



# ----------
dat <- list(lag = matrix(0:5, nrow(chicago), 6, byrow=TRUE),
            death = chicago$death, time = chicago$time)

dat$pm10 <- lagard(chicago$pm10median)

dat$tmp <- lagard(chicago$tmpd)

dat$o3 <- lagard(chicago$o3median)



# ----------
# Function to re-weight the lagged covariates and perform the GAM fitting call using the re-weighted data
# theta:  weighting parameter
si <- function(theta, dat, opt = TRUE){
  # Return ML if opt == TRUE or fitted gam otherwise

  # make identifiable by insisting that ||alpha|| = 1
  alpha <- c(1, theta)  # alpha defined via unconstrained theta
  kk <- sqrt(sum(alpha^2))
  alpha <- alpha / kk  # ||alpha|| = 1

  
  # ----------
  # re-weight lagged covariates
  o3 <- dat$o3 %*% alpha
  tmp <- dat$tmp %*% alpha
  pm10 <- dat$pm10 %*% alpha


  # ----------
  # bam with default REML smoothing parameter estimation in order to keep the computation time reasonable
  # using the discrete = TRUE option to bam would speed up a little more again.
  b <- bam(dat$death ~ s(dat$time, k = 200, bs = "cr") + s(pm10, bs = "cr") + te(o3, tmp, k = 8), family = poisson)  # fit model
  # give user somthing to watch
  cat(".")
  
  # ----------
  if(opt) return(b$gcv.ubre) else{
    b$alpha <- alpha  # add alpha to model object
    b$J <- outer(alpha, -theta / kk^2)  # get dalpha_i / dtheta_j
    for(j in 1:length(theta)) b$J[j+1, j] <- b$J[j+1, j] + 1/kk
    return(b)
  }
}



# ----------
# IT TAKES TIME !!!:  around 30 min.
f1 <- optim(rep(1, 5), si, method = "BFGS", hessian = TRUE, dat = dat)


f1


# -->
# The fairly small negative coefficient for lag 5 could indicate that increases are somewhat harmful, but could also just be artefact:
# there might be a case for constraining the alphaj to be non-negative here.



# ----------
# extract final fitted model
apsi <- si(f1$par, dat, opt = FALSE)



# ----------
apsi$alpha


# -->
# Almost no effect is estimated for the temperature on the day of death, the preceding two days are important and the two days before that less so.



# ----------
gam.check(apsi)


# -->
# much improved relative to the previous models.



# ----------
plot(apsi)


# -->
# Note that the rug plot at the bottom of the time effect now has gaps; 
# this is because observations with any NAs in the lagged variables now have to be dropped
# (the high observations that caused the original bad fits are not dropped this way)

# Most noticeable effect is the big increase in risk when the weighted sums of ozone and temperature are simultaneously high.

# The residula plot now looks much improved: there is one large negative outlier,
# but it is nothing like the magnitude of the previous models' outliers.



# ----------
# vis.gam(apsi, plot.type = "contour", view = c("o3", "tmp"))

