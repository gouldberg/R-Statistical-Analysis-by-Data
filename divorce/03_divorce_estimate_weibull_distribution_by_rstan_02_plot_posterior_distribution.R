# setwd("//media//kswada//MyFiles//R//divorce//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//divorce//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  divorce
# ------------------------------------------------------------------------------

source("divorce_data.R")

length(dat)

data.ls <- list(J = length(dat), tt = dat)

table(dat)



# ------------------------------------------------------------------------------
# plot posterior distribution
# ------------------------------------------------------------------------------

print(fit, pars=par, digits_summary=dig)



# -->
# very close to gamlss estimation



# ----------
# probability density function
m <- extract(fit)$m

eta <- extract(fit)$eta

curve(dweibull(x, shape=round(mean(m),3), scale=round(mean(eta),3)),
      xlim=c(0,408), ylim=c(0,.006), lty=1, lwd=3, ylab="", xlab="x", cex.axis=1.6, cex.lab=1.6)


# ----------
# hazard function
curve(dweibull(x,shape=round(mean(m),3), scale=round(mean(eta),3)) / (1-pweibull(x,shape=round(mean(m),3), scale=round(mean(eta),3))),
      xlim=c(0,408), ylim=c(0,0.02), lty=1,lwd=3, ylab="", cex.axis=1.6, cex.lab=1.6)



# ----------
# average length of period to live together:  131.215 months (10 years 11 months)

# the model of divorce occurring is:  mamou-koshou-gata (mean of "m" = 1.283 > 1 and its 95% of credible interval is 1.276 - 1.282)

# the years to divorce most likely:  mean of mode = 43.556 months (3 - 4 years)
