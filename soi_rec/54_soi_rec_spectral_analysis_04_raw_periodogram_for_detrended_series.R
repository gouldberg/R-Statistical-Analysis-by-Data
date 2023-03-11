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
# detrending the series by fitting a regression of SOI on time t
# ------------------------------------------------------------------------------

lmod <- lm(soi ~ time(soi), data = soi)

soi.res <- ts(resid(lmod))


summary(lmod)


# ----------
par(mfrow = c(2,1))
plot(soi)
abline(lmod, col = "blue", lty = 2)
plot(soi.res, type = "l")
abline(h = 0, col = "blue", lty = 2)



# ------------------------------------------------------------------------------
# Periodograms for the deterended series
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(2,1))

soi.smo <- astsa::mvspec(soi, log = "yes")
abline(v = c(0.25, 1), lty = 2)

soi.smo.res <- astsa::mvspec(soi.res, log = "yes")

