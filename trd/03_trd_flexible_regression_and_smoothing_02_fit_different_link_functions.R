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
# Fit different link functions
# ------------------------------------------------------------------------------

show.link(BI)


# ----------
t2_logit <- gamlss(y ~ pbc(ti, inter = 30), family = BI(mu.link = "logit"))

t2_probit <- gamlss(y ~ pbc(ti, inter = 30), family = BI(mu.link = "probit"))

t2_cloglog <- gamlss(y ~ pbc(ti, inter = 30), family = BI(mu.link = "cloglog"))

# cauchit link does not properly model by pbc()
# t2_cauchit <- gamlss(y ~ pbc(ti, inter = 30), family = BI(mu.link = "cauchit"))

t2_log <- gamlss(y ~ pbc(ti, inter = 30), family = BI(mu.link = "log"))



# ----------
GAIC(t2_logit, t2_probit, t2_cloglog, t2_log, k = log(length(y)))



# -->
# log link is the best



# ----------
plot(t2_log)

plot(t2_probit)



# ------------------------------------------------------------------------------
# Check the residulas
# ------------------------------------------------------------------------------

# Since the fitted distribution is binomial, use rqres.plot() to obtain multiple realizations of the residuals

rqres.plot(t2_log)

rqres.plot(t2_probit)

rqres.plot(t2_log)



# ----------
# worm plot
wp(t2_log)

wp(t2_probit)




# ------------------------------------------------------------------------------
# Compare the fitted predictors
# ------------------------------------------------------------------------------

term.plot(t2_log, pages = 1, ask = FALSE)

term.plot(t2_probit, pages = 1, ask = FALSE)



# ----------
# plot the data and the fitted mu
par(mfrow=c(1,1))
plot(y[,"trd"]/2)

lines(fitted(t2_log) ~ ti, col = "blue", lty = 2)

lines(fitted(t2_probit) ~ ti, col = "gray", lty = 3)




