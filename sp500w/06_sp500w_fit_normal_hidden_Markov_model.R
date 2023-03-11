setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
#   - weekly S&P 500 returns from to Jan. 2003 to Sept. 2012
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)




# ------------------------------------------------------------------------------
# Fit normal hidden Markov model
# ------------------------------------------------------------------------------

y <- ts(sp500w, start = 2003, freq = 52)


# depmixS4, uses the EM algorithm, but does not provide standard errors.
library(depmixS4)


model <- depmix(y ~ 1, nstates = 3, data = data.frame(y))


model



# ----------
# Estimation results
set.seed(2)

summary(fm <- fit(model))




# ----------
para.mle <- as.vector(getpars(fm)[-c(1:3)])

# for the label switch
permu <- matrix(c(0, 0, 1, 0, 1, 0, 1, 0, 0), 3, 3)


# transition matrix
( mtrans.mle <- permu %*% round(t(matrix(para.mle[1:9], 3, 3)), 3) %*% permu )


# -->
# note that 2 --> 2 transition is almost 0
# and 1 --> 3 is almost 0, too.



# 3 fitted normals: N(mu1 = 0.04, sigma1 = 0.14),  N(mu2 = -0.034, sigma2 = 0.009),  N(mu3 = -0.003, sigma3 = 0.044)
( norms.mle <- round(matrix(para.mle[10:15], 2, 3), 3) %*% permu )



