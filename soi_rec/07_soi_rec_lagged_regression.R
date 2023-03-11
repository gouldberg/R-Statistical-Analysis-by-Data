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
# Again check cross-correlation
# ------------------------------------------------------------------------------


ccf2(soi, rec)




# ------------------------------------------------------------------------------
# Regression with lagged variables with align lagged series
# ------------------------------------------------------------------------------

# align the lagged series
( fish <- ts.intersect(rec, soiL6 = stats::lag(soi, -6), dframe = TRUE) )



# ----------
# Regress on only lag(soi, -6)
fit1 <- lm(rec ~ soiL6, data = fish, na.action = NULL)


summary(fit1)


par(mfrow = c(2,2), mar = c(2,2,2,2))

plot(fit1)




# ------------------------------------------------------------------------------
# Regression with lagged variables:  no need to align lagged series
# ------------------------------------------------------------------------------

# The headache of aligning the lagged series can be avoided by using dynlm
library(dynlm)


# dynlm model have time series attributes without any additional commands
fit2 <- dynlm(rec ~ L(soi, 6))


summary(fit2)



# ----------
plot(fit2)
