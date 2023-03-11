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
# data exploration:  fit distributions automatically
# ------------------------------------------------------------------------------


library(gamlss)


f1 <- fitDist(EQcount, type = "counts", k = 2, trace = TRUE)



# ----------
# shows GAIC from best model to worst model
# PIG:  Poisson Inverse Gaussian (2 parameters)
# (2nd: Generalized Poisson  3rd:  Negative Binomial TYpe II)

f1$fits




# ----------
# No fails
f1$fails




# ----------
# Repeat with k = 3.84 and k = log(length(Rdax)) corresponding to criteria X^2(1, 0.05) and Schwartz Bayesian Criteria (SBC), respectively

f2 <- fitDist(EQcount, type = "counts", k = 3.84, trace = TRUE)

f3 <- fitDist(EQcount, type = "counts", k = log(length(EQcount)), trace = TRUE)


f2$fits
f2$fails


f3$fits
f3$fails



# ----------
# plot the best PIG

par(mfrow = c(1,1))
histDist(EQcount, family = "PIG", nbins = 30, line.wd = 2.5)




# ----------
# refit the model using gamlss() in order to output the parameter estimates using summary() 
f1_2 <- gamlss(EQcount ~ 1, family = "PIG")

summary(f1_2)




# ----------
wp(f1_2)

