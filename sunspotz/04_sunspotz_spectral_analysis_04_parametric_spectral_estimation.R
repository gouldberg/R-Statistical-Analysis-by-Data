setwd("//media//kswada//MyFiles//R//sunspotz")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sunspotz
# ------------------------------------------------------------------------------

data(sunspotz, package = "astsa")

str(sunspotz)

head(sunspotz)



# ------------------------------------------------------------------------------
# Find best autoregression order by by AIC, AICc, BIC
# ------------------------------------------------------------------------------

n <- length(sunspotz)

AIC <- rep(0, 40) -> AICc -> BIC


# var.pred:  the prediction variance, an estimate of the portion of the variance of the time series
# that is not explained by the autoregressive model
for(k in 1:40){
  sigma2 <- ar(sunspotz, order = k, aic = FALSE)$var.pred
  BIC[k] <- log(sigma2) + (k * log(n) / n)
  AICc[k] <- log(sigma2) + ((n + k) / (n - k - 2))
  AIC[k] <- log(sigma2) + ((n + 2 * k) / n)
}


# AICc is nearly identical to BIC, so we plot only AIC and BIC
# We added +1 to the BIC to reduce white space in the graphic
IC <- cbind(AIC, BIC + 1)

ts.plot(IC, type =c("o"), xlab = "p", ylab = "AIC / BIC")



# ------------------------------------------------------------------------------
# Spectral analysis:  Autoregressive spectral estimator for sunspotz
#   - Often, the periodicities in the sunspot series are investigated by fitting an autoregressive spectrum of sufficiently high order.
# ------------------------------------------------------------------------------

graphics.off()


# spec.ar() to fit the best model via AIC and plot the resulting spectrum
spaic <- spec.ar(sunspotz, log = "no")


# ----------
spaic$method


# -->
# AR(16) spectrum


# ----------
which.max(spaic$spec)

spaic$freq[which.max(spaic$spec)]

1 / spaic$freq[which.max(spaic$spec)]


# -->
# 10.4 years per cycle is the most predominant cycle



# ----------
# estimates and AICs
( sun.ar <- ar(sunspotz, order.max = 40) )

sun.ar$aic

plot(1:40, sun.ar$aic[-1], type = "o")


# -->
# No likelihood is calculated here, so the use of the term AIC is loose.

