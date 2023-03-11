setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")

str(econ5)


car::some(econ5)




# ----------
# data transformation
# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))


x <- cbind(G, U, C)



# ----------
Ud <- diff(econ5$unemp)

Gd <- diff(econ5$govinv)

Cd <- diff(econ5$consum)



dat <- cbind(Gd, Ud, Cd)




# ------------------------------------------------------------------------------
# Find best autoregression order by by AIC, AICc, BIC
# ------------------------------------------------------------------------------

n <- length(Ud)


AIC <- rep(0, 40) -> AICc -> BIC


# var.pred:  the prediction variance, an estimate of the portion of the variance of the time series
# that is not explained by the autoregressive model
for(k in 1:40){
  sigma2 <- ar(Ud, order = k, aic = FALSE)$var.pred
  BIC[k] <- log(sigma2) + (k * log(n) / n)
  AICc[k] <- log(sigma2) + ((n + k) / (n - k - 2))
  AIC[k] <- log(sigma2) + ((n + 2 * k) / n)
}



# ----------
# AICc is nearly identical to BIC, so we plot only AIC and BIC
# We added +1 to the BIC to reduce white space in the graphic

IC <- cbind(AIC, BIC + 1)

ts.plot(IC, type =c("o"), xlab = "p", ylab = "AIC / BIC")




# ------------------------------------------------------------------------------
# SOI  Spectral analysis:  Autoregressive spectral estimator
# ------------------------------------------------------------------------------

graphics.off()


# spec.ar() to fit the best model via AIC and plot the resulting spectrum
spaic <- spec.ar(Ud, log = "no")




# ----------
spaic$method



# -->
# AR(4) spectrum



# ----------
# estimates and AICs
( Ud.ar <- ar(Ud, order.max = 40) )

Ud.ar$aic


plot(1:40, Ud.ar$aic[-1], type = "o")



# -->
# No likelihood is calculated here, so the use of the term AIC is loose.




# ------------------------------------------------------------------------------
# Gd  Spectral analysis:  Autoregressive spectral estimator
# ------------------------------------------------------------------------------

graphics.off()


spaic <- spec.ar(Gd, log = "no")

spaic <- spec.ar(Cd, log = "no")


# ----------
spaic$method


# -->
# AR(3) spectrum

