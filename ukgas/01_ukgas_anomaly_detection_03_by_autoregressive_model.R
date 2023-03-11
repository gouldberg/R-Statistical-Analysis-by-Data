
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UKgas
# ------------------------------------------------------------------------------


data(UKgas)


str(UKgas)


UKgas



# ----------
dat2 <- diff(log(UKgas))




# ------------------------------------------------------------------------------
# Estimate Autoregressive Model for Training Data
# ------------------------------------------------------------------------------


# training data
Dtr <- dat2[1:20]


# validation data
xi <- dat2[21:length(dat2)]



Tt <- length(xi)



# ----------
# AR model, automatically selecting AR orders  --> AR(2)

ar.model <- ar(Dtr)


print(ar.model)




# ------------------------------------------------------------------------------
# Compute predicted values ofr validation data set 
# ------------------------------------------------------------------------------

( r <- ar.model$order )


alpha <- ar.model$ar


xmean <- ar.model$x.mean


sig2 <- ar.model$var.pred




# ----------
# compute predicted values for validation data (by each data point)

N <- Tt - r


X <- t(embed(xi - xmean, r))[,1:N]


dim(X)


ypred <- t(X) %*% alpha + xmean




# ------------------------------------------------------------------------------
# Compute anomaly score
# ------------------------------------------------------------------------------


y <- xi[(1 + r):Tt]



# anomaly score
a <- (y - as.numeric(ypred))^2 / sig2




# ----------

graphics.off()

par(mfrow = c(2,1))


plot(xi, type = "l", main = "original validation series")
lines(c(rep(NA, 2), ypred), lty = 1, col = "blue", lwd = 2)

plot(c(rep(NA, 2), a), type = "l", lty = 1, col = "blue", main = "anomaly score")





