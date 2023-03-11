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
# Find best autoregression order by by AIC, AICc, BIC
# ------------------------------------------------------------------------------

n <- length(soi)


AIC <- rep(0, 40) -> AICc -> BIC


# var.pred:  the prediction variance, an estimate of the portion of the variance of the time series
# that is not explained by the autoregressive model
for(k in 1:40){
  sigma2 <- ar(soi, order = k, aic = FALSE)$var.pred
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
spaic <- spec.ar(soi, log = "no")


# El Nino peak
abline(v = frequency(soi)*1/52, lty = 3)



# ----------
spaic$method


# -->
# AR(15) spectrum



# ----------
# estimates and AICs
( soi.ar <- ar(soi, order.max = 40) )

soi.ar$aic


plot(1:40, soi.ar$aic[-1], type = "o")


# -->
# No likelihood is calculated here, so the use of the term AIC is loose.




# ------------------------------------------------------------------------------
# REC  Spectral analysis:  Autoregressive spectral estimator
# ------------------------------------------------------------------------------

graphics.off()


spaic <- spec.ar(rec, log = "no")


abline(v = c(0.25, 1), lty = 2)



# ----------
spaic$method


# -->
# AR(13) spectrum

