rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\nottem")




# ------------------------------------------------------------------------------
# data:  nottem
#   - monthly temperature from 1920 to 1939 in Nottingham Castle in England
# ------------------------------------------------------------------------------


data(nottem)


str(nottem)



# ----------
par(mfrow = c(1,1))

plot(nottem, type = "l")




# ------------------------------------------------------------------------------
# Estimate Autoregressive Model for Training Data
# ------------------------------------------------------------------------------


# training data
Dtr <- nottem[1:120]


# validation data
xi <- nottem[121:240]



Tt <- length(xi)



# ----------
# AR model, automatically selecting AR orders  --> AR(8)

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

rng <- c(43, 49, 55, 60, 66, 76, 87, 97)

plot(xi, type = "l", main = "original validation series")
lines(c(rep(NA, 8), ypred), lty = 1, col = "blue", lwd = 2)
abline(v = rng, lty = 2, col = "red")

plot(c(rep(NA, 8), a), type = "l", lty = 1, col = "blue", main = "anomaly score")
abline(v = rng, lty = 2, col = "red")





