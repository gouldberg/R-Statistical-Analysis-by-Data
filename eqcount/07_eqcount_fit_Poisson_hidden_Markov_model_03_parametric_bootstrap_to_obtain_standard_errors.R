setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")


str(EQcount)


EQcount




# ------------------------------------------------------------------------------
# Parametric bootstrap to obtain standard errors
# ------------------------------------------------------------------------------

# n: data length
# m: number of states
# Mtrans: transition matrix
# StatDist: stationary distn

pois.HMM.generate_sample <- function(n, m, lambda, Mtrans, StatDist = NULL){
  
  if(is.null(StatDist)) StatDist <- solve(t(diag(m) - Mtrans + 1), rep(1, m))
  
  mvect <- 1:m
  
  state <- numeric(n)
  
  state[1] <- sample(mvect, 1, prob = StatDist)
  
  for(i in 2:n) state[i] <- sample(mvect, 1, prob = Mtrans[state[i-1], ])
  
  y <- rpois(n, lambda = lambda[state])
  
  list(y = y, state = state)
}



# ----------
# start it up

set.seed(10101101)

nboot <- 1000

nobs <- length(EQcount)

para.star <- matrix(NA, nrow = nboot, ncol = 6)

for(j in 1:nboot){
  
  x.star <- pois.HMM.generate_sample(n = nobs, m = 2, lambda = lams, Mtrans = mtrans)$y
  
  model <- depmix(x.star ~ 1, nstates = 2, data = data.frame(x.star), family = poisson())
  
  u <- as.vector(getpars(fit(model, verbose = 0)))
  
  # make sure state 1 is the one with the smaller intensity parameter
  if(u[7] <= u[8]){
    para.star[j,] <- c(u[3:6], exp(u[7]), exp(u[8]))
  } else { para.star[j,] <- c(u[6:3], exp(u[8]), exp(u[7])) }
  
}


para.mle



# ----------
# bootstrapped std errors
SE <- sqrt(apply(para.star, 2, var) + (apply(para.star, 2, mean) - para.mle)^2)[c(1,4:6)]

names(SE) <- c("seM11 / M12", "seM21 / M22", "seLam1", "seLam2")


SE



# -->
# seM11 / M12:  standard errors for transition probability
# seLam1:  standard errors of lambda for 1st state



