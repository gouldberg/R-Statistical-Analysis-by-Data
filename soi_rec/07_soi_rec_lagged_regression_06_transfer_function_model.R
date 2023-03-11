setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data:  rec
# ------------------------------------------------------------------------------

data(soi, package = "astsa")


data(rec, package = "astsa")




# ------------------------------------------------------------------------------
# Regression with lagged variables
# Transfer function model:  parsimonious model
# ------------------------------------------------------------------------------


fish <- ts.intersect(R = rec, RL1 = stats::lag(rec, -1),  SL5 = stats::lag(soi.d, -5))

( u <- lm(fish[,1] ~ fish[,2:3], na.action = NULL) )



acf2(resid(u))




# ----------
# final model
arx <- sarima(fish[,1], 1, 0, 0, xreg = fish[,2:3])




# ----------
arx$AICc




# ------------------------------------------------------------------------------
# 1-step-ahead predictions
# ------------------------------------------------------------------------------

pred <- rec + resid(arx$fit)

par(mfrow = c(1,1))

ts.plot(pred, rec, col = c('skyblue', "black"), lwd = c(7,2))


