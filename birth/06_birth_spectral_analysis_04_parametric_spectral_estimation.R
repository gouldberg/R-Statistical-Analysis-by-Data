setwd("//media//kswada//MyFiles//R//birth")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birth
# ------------------------------------------------------------------------------

data(birth, package = "astsa")

str(birth)

head(birth)



# ------------------------------------------------------------------------------
# Find best autoregression order by by AIC, AICc, BIC
# ------------------------------------------------------------------------------

n <- length(log(birth))

AIC <- rep(0, 40) -> AICc -> BIC


# var.pred:  the prediction variance, an estimate of the portion of the variance of the time series
# that is not explained by the autoregressive model
for(k in 1:40){
  sigma2 <- ar(log(birth), order = k, aic = FALSE)$var.pred
  BIC[k] <- log(sigma2) + (k * log(n) / n)
  AICc[k] <- log(sigma2) + ((n + k) / (n - k - 2))
  AIC[k] <- log(sigma2) + ((n + 2 * k) / n)
}


# AICc is nearly identical to BIC, so we plot only AIC and BIC
# We added +1 to the BIC to reduce white space in the graphic
IC <- cbind(AIC, BIC + 1)

ts.plot(IC, type =c("o"), xlab = "p", ylab = "AIC / BIC")



# ------------------------------------------------------------------------------
# Spectral analysis:  Autoregressive spectral estimator
# ------------------------------------------------------------------------------

graphics.off()


# spec.ar() to fit the best model via AIC and plot the resulting spectrum
spaic <- spec.ar(log(UnempRate), log = "no")


# ----------
spaic$method



