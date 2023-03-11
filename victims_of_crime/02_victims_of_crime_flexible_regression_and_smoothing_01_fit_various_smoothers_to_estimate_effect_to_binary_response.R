setwd("//media//kswada//MyFiles//R//victims_of_crime")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Victims of Crime
#  - Variables
#       - reported:  whether the crime was reported in local media (0 = no, 1 = yes)
#       - age:  age of the victim
# ------------------------------------------------------------------------------
data("VictimsOfCrime", package = "gamlss.data")


str(VictimsOfCrime)

car::some(VictimsOfCrime)




# ------------------------------------------------------------------------------
# Fit various smoothers
# ------------------------------------------------------------------------------

# This data has 10590 observations.  So user the local SBC method for a smoother fitted curve

library(gamlss.add)


# pb():  penalized B-splines
m_bsp <- gamlss(reported ~ pb(age, method = "GAIC", k = 2), data = VictimsOfCrime, family = BI)
m_bsp_s <- gamlss(reported ~ pb(age, method = "GAIC", k = log(length(VictimsOfCrime$age))), data = VictimsOfCrime, family = BI)


# pbm(): P-splines monotonic
m_pbm <- gamlss(reported ~ pbm(age, mono = "down", method = "GAIC", k = 2), data = VictimsOfCrime, family = BI)
m_pbm_s <- gamlss(reported ~ pbm(age, mono = "down", method = "GAIC", k = log(length(VictimsOfCrime$age))), data = VictimsOfCrime, family = BI)


# cy(): P-splines cyclic
m_cy <- gamlss(reported ~ cy(age, method = "GAIC", k = 2), data = VictimsOfCrime, family = BI)
m_cy_s <- gamlss(reported ~ cy(age, method = "GAIC", k = log(length(VictimsOfCrime$age))), data = VictimsOfCrime, family = BI)


# scs(): cubic splines with automatic GCV
m_scs <- gamlss(reported ~ scs(age, method = "GAIC", k = 2), data = VictimsOfCrime, family = BI)
m_scs_s <- gamlss(reported ~ scs(age, method = "GAIC", k = log(length(VictimsOfCrime$age))), data = VictimsOfCrime, family = BI)


# lo(): loess
m_lo <- gamlss(reported ~ lo(~ age, method = "GAIC", k = 2), data = VictimsOfCrime, family = BI)
m_lo_s <- gamlss(reported ~ lo(~ age, method = "GAIC", k = log(length(VictimsOfCrime$age))), data = VictimsOfCrime, family = BI)


# nn(): neural networks
# m_nn <- gamlss(reported ~ nn(~ age, size = 5, decay = 0.01), data = VictimsOfCrime, family = BI)
m_nn <- gamlss(reported ~ nn(~ age), data = VictimsOfCrime, family = BI)
term.plot(m_nn, pages = 1, ask = FALSE)



# tr(): decision trees
m_tr <- gamlss(reported ~ tr(~ age), data = VictimsOfCrime, family = BI)



# ----------
# compare models by SBC
GAIC(m_bsp, m_bsp_s, m_pbm, m_pbm_s, m_cy, m_cy_s, m_scs, m_scs_s, m_lo, m_lo_s, m_nn, m_tr, k = log(length(VictimsOfCrime$age)))




# ------------------------------------------------------------------------------
# Compare fitted mu (probability of a crime being reported in local media)
# ------------------------------------------------------------------------------
par(mfrow = c(2,2))

# Compare the behavior of P-spline based curves
plot(reported ~ age, data = VictimsOfCrime, pch = "|", col = gray(0.7))
with(VictimsOfCrime, lines(fitted(m_bsp_s)[order(age)] ~ age[order(age)], col = "black", lty = 1, lwd = 2))
with(VictimsOfCrime, lines(fitted(m_pbm_s)[order(age)] ~ age[order(age)], col = "red", lty = 1, lwd = 2))
with(VictimsOfCrime, lines(fitted(m_cy_s)[order(age)] ~ age[order(age)], col = "blue", lty = 1, lwd = 2))


# Compare the behavior of P-splines and cubic splines
plot(reported ~ age, data = VictimsOfCrime, pch = "|", col = gray(0.7))
with(VictimsOfCrime, lines(fitted(m_bsp_s)[order(age)] ~ age[order(age)], col = "black", lty = 1, lwd = 2))
with(VictimsOfCrime, lines(fitted(m_scs_s)[order(age)] ~ age[order(age)], col = "skyblue", lty = 1, lwd = 2))


# Compare the behavior of P-splines, loess, neural network and decision trees
plot(reported ~ age, data = VictimsOfCrime, pch = "|", col = gray(0.7))
with(VictimsOfCrime, lines(fitted(m_bsp_s)[order(age)] ~ age[order(age)], col = "black", lty = 1, lwd = 2))
with(VictimsOfCrime, lines(fitted(m_lo_s)[order(age)] ~ age[order(age)], col = "red", lty = 1, lwd = 2))
with(VictimsOfCrime, lines(fitted(m_nn)[order(age)] ~ age[order(age)], col = "blue", lty = 1, lwd = 2))
with(VictimsOfCrime, lines(fitted(m_tr)[order(age)] ~ age[order(age)], col = "skyblue", lty = 1, lwd = 2))




# ------------------------------------------------------------------------------
# Compare fitted mu with 95% interval by term.plot
# ------------------------------------------------------------------------------

term.plot(m_bsp_s, pages = 1, ask = FALSE)

term.plot(m_scs_s, pages = 1, ask = FALSE)

term.plot(m_cy_s, pages = 1, ask = FALSE)

term.plot(m_cy_s, pages = 1, ask = FALSE)



# ------------------------------------------------------------------------------
# Check the residuals
# ------------------------------------------------------------------------------

# Note that for binary responses, the function rqres.plot() returns multiple realizations of the residuals

plot(m_bsp_s)

rqres.plot(m_bsp_s, ylin.all = .6)



# ----------
# obtain a multiple worm plot of the residuals
wp(m_bsp_s, xvar = age, n.inter = 9)

