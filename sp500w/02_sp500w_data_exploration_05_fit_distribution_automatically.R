setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)


# convert to ts object
sp500w_c <- ts(sp500w, start = 2003, freq = 52)




# ------------------------------------------------------------------------------
# data exploration:  fit distributions automatically
# ------------------------------------------------------------------------------


library(gamlss)


f1 <- fitDist(sp500w, type = "realline", k = 2, trace = TRUE)



# ----------
# shows GAIC from best model to worst model
# JSUo:  Original Johnson's Su Distribution  (4 parameter model)

f1$fits




# ----------
# No fails
f1$fails




# ----------
# Repeat with k = 3.84 and k = log(length(Rdax)) corresponding to criteria X^2(1, 0.05) and Schwartz Bayesian Criteria (SBC), respectively

f2 <- fitDist(sp500w, type = "realline", k = 3.84, trace = TRUE)

f3 <- fitDist(sp500w, type = "realline", k = log(length(sp500w)), trace = TRUE)


f2$fits
f2$fails


f3$fits
f3$fails



# ----------
# plot the best JSUo

histDist(sp500w, family = "JSUo", nbins = 30, line.wd = 2.5)



# ----------
# refit the model using gamlss() in order to output the parameter estimates using summary() 
f1_2 <- gamlss(sp500w ~ 1, family = "JSUo")

summary(f1_2)




# ----------
wp(f1_2)

