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
# Forecasting the Recruitment series by AR(2) model by Yule-Walker estimation
#   - In the case of AR(p) models, the Yule-Walker estimators (this is method of moments estimator) are optimal in the sense
#     that the asymptotic distribution (of sqrt(n) * (phi-hat - phi)) is the best asymptotic normal distribution.
#     This is because, given initial condistions, AR(p) models are linear models, and the Yule-Walker estimators are essentially least squares estimators.
#     If we use method of moments for MA or ARMA models, we will not gt optimal estimators because such processes are nonlinear in the parameters.
# ------------------------------------------------------------------------------

rec.yw <- ar.yw(rec, order = 2)



# ----------
# Yule-Walker estimates are nearly identical, here

# mean estimate
rec.yw$x.mean


# coefficient estimates
rec.yw$ar


# standard errors
sqrt(diag(rec.yw$asy.var.coef))


# error variance estimate
rec.yw$var.pred



# ----------
# obtain the 24 month ahead predictions and their standard error
rec.pr <- predict(rec.yw, n.ahead = 24)

ts.plot(rec, rec.pr$pred, col = 1:2)

lines(rec.pr$pred + rec.pr$se, col = 4, lty = 2)

lines(rec.pr$pred - rec.pr$se, col = 4, lty = 2)


