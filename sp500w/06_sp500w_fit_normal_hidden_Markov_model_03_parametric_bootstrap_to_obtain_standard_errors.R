setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)



# ------------------------------------------------------------------------------
# Parametric bootstrap to obtain standard errors
# ------------------------------------------------------------------------------

set.seed(666)

nboot <- 100

nobs <- length(y)

para.star <- matrix(NA, nrow = nboot, ncol = 15)

( respst <- para.mle[10:15] )

( trst <- para.mle[1:9] )




# ----------

for(j in 1:nboot){
  
  mod <- simulate(model)

  y.star <- as.vector(mod@response[[1]][[1]]@y)
  
  dfy <- data.frame(y.star)
  
  mod.star <- depmix(y.star ~ 1, data = dfy,  respst = respst, trst = trst, nst = 3)
  
  fm.star <- fit(mod.star, emcontrol = em.control(tol = 1e-5), verbose = FALSE)
  
  para.star[j,] <- as.vector(getpars(fm.star)[-(1:3)])

}


para.star



# ----------
# bootstrapped std errors
SE <- sqrt(apply(para.star, 2, var) + (apply(para.star, 2, mean) - para.mle)^2)




# ----------
mtrans.mle
( SE.mtrans.mle <- permu %*% round(t(matrix(SE[1:9], 3, 3)), 3) %*% permu )



norms.mle
( SE.norms.mle <- round(matrix(SE[10:15], 2, 3), 3) %*% permu )



# -->
# States 1 and 3 represnet clusters of regular or high volatility, respectively.
# Note that there is a large amount of uncertainty in the fitted normals,
# and in the transition matrix involving transitions from state 2 to states 1 or 3.

