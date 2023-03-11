setwd("//media//kswada//MyFiles//R//eqexp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqexp
# ------------------------------------------------------------------------------

data(eqexp, package = "astsa")


head(eqexp)

str(eqexp)


attach(eqexp)


# ----------
P <- 1:1024

S <- P + 1024


x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S], EX6[S], NZ[S])

x.name <- c("EQ5-P", "EQ6-P", "EX5-P", "EX6-P", "NZ-P", "EQ5-S", "EQ6-S", "EX5-S", "EX6-S", "NZ-S")

colnames(x) <- x.name



# ------------------------------------------------------------------------------
# Find best autoregression order by by AIC, AICc, BIC
# ------------------------------------------------------------------------------

n <- length(EQ5[P])

AIC <- rep(0, 40) -> AICc -> BIC


# var.pred:  the prediction variance, an estimate of the portion of the variance of the time series
# that is not explained by the autoregressive model
for(k in 1:40){
  # sigma2 <- ar(EQ5[P], order = k, aic = FALSE)$var.pred
  sigma2 <- ar(EQ5[S], order = k, aic = FALSE)$var.pred
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

par(mfrow = c(1,2))


# spec.ar() to fit the best model via AIC and plot the resulting spectrum


# ----------
# Earth Quake

spaic <- spec.ar(EQ5[P], log = "no")
abline(v = c(0.018, 0.095), lty = 3)


spaic2 <- spec.ar(EQ5[S], log = "no")
abline(v = 0.031, lty = 3)




# ----------
# Explosion

spaic3 <- spec.ar(EX5[P], log = "no")

spaic4 <- spec.ar(EX5[S], log = "no")




# ----------
# NZ

spaic5 <- spec.ar(NZ[P], log = "no")

spaic6 <- spec.ar(NZ[S], log = "no")




# ----------
spaic$method
spaic2$method


# -->
# AR(24) spectrum
# AR(18) spectrum



# ----------
# estimates and AICs
( eqp.ar <- ar(EQ5[P], order.max = 40) )

eqp.ar$aic

plot(1:40, eqp.ar$aic[-1], type = "o")


# -->
# No likelihood is calculated here, so the use of the term AIC is loose.

