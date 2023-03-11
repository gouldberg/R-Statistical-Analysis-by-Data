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
# Forecasting the Recruitment series by AR(2) model by maximum likelihood estimation
# ------------------------------------------------------------------------------

rec.mle <- ar.mle(rec, order = 2)


# ----------
# mean estimate
rec.mle$x.mean


# coefficient estimates
rec.mle$ar


# standard errors
sqrt(diag(rec.mle$asy.var.coef))


# error variance estimate
rec.mle$var.pred



# ----------
# obtain the 24 month ahead predictions and their standard error
rec.pr2 <- predict(rec.mle, n.ahead = 24)

ts.plot(rec, rec.pr2$pred, col = 1:2)

lines(rec.pr2$pred + rec.pr2$se, col = 4, lty = 2)

lines(rec.pr2$pred - rec.pr2$se, col = 4, lty = 2)


