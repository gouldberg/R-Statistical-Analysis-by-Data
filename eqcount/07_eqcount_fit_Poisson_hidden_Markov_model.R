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
# Fit Poisson hidden Markov model
# ------------------------------------------------------------------------------

# depmixS4, uses the EM algorithm, but does not provide standard errors.

library(depmixS4)


model <- depmix(EQcount ~ 1, nstates = 2, data = data.frame(EQcount), family = poisson())


model




# ----------
# Estimation results
set.seed(90210)

summary(fm <- fit(model))




# ----------
# Get parameters

# ensure state 1 has smaller lambda
u <- as.vector(getpars(fm))


if(u[7] <= u[8]){
  para.mle <- c(u[3:6], exp(u[7]), exp(u[8]))
} else {
  para.mle <- c(u[6:3], exp(u[8]), exp(u[7]))  
}


( mtrans <- matrix(para.mle[1:4], byrow = TRUE, nrow = 2) )

( lams <- para.mle[5:6] )



# ----------
# ratio of mixtures

( pi1 <- mtrans[2,1] / ( 2 - mtrans[1,1] - mtrans[2,2] ) )


( pi2 <- 1 - pi1 )

