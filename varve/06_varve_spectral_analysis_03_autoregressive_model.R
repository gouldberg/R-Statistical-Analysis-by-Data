setwd("//media//kswada//MyFiles//R//varve")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  varve
# ------------------------------------------------------------------------------

data(varve, package = "astsa")

str(varve)



# ------------------------------------------------------------------------------
# Find best autoregression order by by AIC, AICc, BIC
# ------------------------------------------------------------------------------

# var.pred:  the prediction variance, an estimate of the portion of the variance of the time series
# that is not explained by the autoregressive model

n <- length(varve)

AIC <- rep(0, 40) -> AICc -> BIC


for(k in 1:40){
  sigma2 <- ar(log(varve), order = k, aic = FALSE)$var.pred
  
  BIC[k] <- log(sigma2) + (k * log(n) / n)
  
  AICc[k] <- log(sigma2) + ((n + k) / (n - k - 2))
  
  AIC[k] <- log(sigma2) + ((n + 2 * k) / n)
}



# ----------
# AICc is nearly identical to BIC, so we plot only AIC and BIC
# We added +1 to the BIC to reduce white space in the graphic

IC <- cbind(AIC, BIC + 1)

ts.plot(IC, type =c("o"), xlab = "p", ylab = "AIC / BIC")
  

# -->
# order = 8 is minimum



# ------------------------------------------------------------------------------
# Spectral analysis:  Autoregressive spectral estimator for log(varve)
# ------------------------------------------------------------------------------

graphics.off()


# spec.ar() to fit the best model via AIC and plot the resulting spectrum
spaic <- spec.ar(log(varve), log = "no")



# ----------
spaic$method


# -->
# AR(8) spectrum



# ----------
# estimates and AICs
( varve.ar <- ar(log(varve), order.max = 40) )

varve.ar$aic

plot(1:40, varve.ar$aic[-1], type = "o")


# -->
# No likelihood is calculated here, so the use of the term AIC is loose.



# ----------
# coeffs and error variance sigma^2 = 0.23
varve.ar

ar.mle(log(varve))



# ------------------------------------------------------------------------------
# Compare spectral estimates of long memory (d = 0.380) and autoregressive AR(8)
# ------------------------------------------------------------------------------

# autoregressive AR(8) spectrum
u <- spec.ar(log(varve), plot = TRUE)

u$method


plot(1:500/2000, log(fhat), type = "l", ylab = "log(spectrum)", xlab = "frequency")

lines(u$freq[1:250], log(u$spec[1:250]), lty = "dashed")



