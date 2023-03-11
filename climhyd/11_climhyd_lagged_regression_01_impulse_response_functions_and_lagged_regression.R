setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)


inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)




# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
#   - The high coherence between sqrt precipitation and logged inflow series suggests
#     a lagged regression relation between the two series.
# ------------------------------------------------------------------------------

graphics.off()


# estimated regression or impulse response function
# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero

L <- 25

mod_lag <- astsa::LagReg(input = prec, output = inf, L = L, M = 100, threshold = 0.01)




# ----------
names(mod_lag)


# beta for s = 0 to 4
mod_lag$betas[50:53,]



mod_lag$fit



# ----------
# residuals
car::scatterplot(time(mod_lag$fit), data.frame(mod_lag$fit)$resids)




# ------------------------------------------------------------------------------
# Examine inverse relation  --> BAD FIT ..
# ------------------------------------------------------------------------------

L <- 25

mod_lag2 <- astsa::LagReg(input = inf, output = prec, L = L, M = 100, inverse = TRUE,  threshold = 0.01)





# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------


inf <- as.ts(inf)

prec <- as.ts(prec)


clim <- ts.intersect(inf = inf, prec = stats::lag(prec, 0), prec1 = stats::lag(prec, 1), 
                     prec2 = stats::lag(prec, 2), prec3 = stats::lag(prec, 3), prec4 = stats::lag(prec, 4))


( u <- lm(clim[,1] ~ clim[,2:6], na.action = NULL) )



summary(u)



coef(u)





