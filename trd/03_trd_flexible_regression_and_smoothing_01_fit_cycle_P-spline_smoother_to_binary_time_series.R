setwd("//media//kswada//MyFiles//R//trd")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  trd (Tokyo rainfall data)
# ------------------------------------------------------------------------------
data("trd", package = "gamlss.data")


str(trd)



# ----------
# Define the binomial response variable
# making sure that 29 Feb was observed ony once
NY <- 2 - trd

NY[60] <- 1 - trd[60]

y <- cbind(trd, NY)

ti <- 1:366



# ------------------------------------------------------------------------------
# Fit a standard and a cycle P-spline smoother
# ------------------------------------------------------------------------------

library(gamlss)


# inter:  the no of break points (knots) in the x-axis

# pb:  P-splines (using SVD)
t1 <- gamlss(y ~ pb(ti, inter = 30), family = BI) 


# pbc:  cycle P-splines (using SVD)
t2 <- gamlss(y ~ pbc(ti, inter = 30), family = BI) 




# ----------
GAIC(t1, t2)


# -->
# t2 is better



# ----------
# model diagnostics
plot(t2)




# ----------
# plot the data and the fitted mu
par(mfrow=c(1,1))
plot(y[,"trd"]/2)

lines(fitted(t1) ~ ti, col = "gray", lty = 3)

lines(fitted(t2) ~ ti, col = "blue", lty = 2)



# ----------
# Compare the fitted predictors
term.plot(t1, pages = 1, ask = FALSE)

term.plot(t2, pages = 1, ask = FALSE)



# ----------
getSmo(t1)

getSmo(t2)


# -->
# The degrees of freedom is smaller for cycle P-spline

