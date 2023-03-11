setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part



# ------------------------------------------------------------------------------
# Residual analysis by sarima() using output from Kfilter2()
# ------------------------------------------------------------------------------

phi1 <- est$par[1]

phi2 <- est$par[2]

cR <- est$par[3]

b1 <- est$par[4]

b2 <- est$par[5]

b3 <- est$par[6]

b4 <- est$par[7]

alf <- est$par[8]

mu0 <- matrix(c(0, 0), 2, 1)

Sigma0 <- diag(100, 2)

Phi <- matrix(c(phi1, phi2, 1, 0), 2)

Theta <- matrix(c(phi1, phi2), 2)

Ups <- matrix(c(b1, 0, b2, 0, b3, 0, 0, 0, 0, 0), 2, 5)

Gam <- matrix(c(0, 0, 0, b4, alf), 1, 5)

cQ <- cR

S <- cR^2



# ----------
kf <- Kfilter2(num, y, A, mu0, Sigma0, Phi, Ups, Gam, Theta, cQ, cR, S, input)



# ----------
res <- ts(as.vector(kf$innov), start = start(cmort), freq = frequency(cmort))



sarima(res, 0, 0, 0, no.constant = TRUE)

