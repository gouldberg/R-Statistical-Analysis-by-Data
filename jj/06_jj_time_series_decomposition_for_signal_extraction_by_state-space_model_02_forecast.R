setwd("//media//kswada//MyFiles//R//jj")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Johnson & Johnson Quarterly Earnings
# ------------------------------------------------------------------------------

data(jj, package = "astsa")

str(jj)

jj


# ----------
x <- jj

lx <- log(x)

dlx <- diff(lx)

ddlx <- diff(dlx, 4)



# ------------------------------------------------------------------------------
# Forecasting
# ------------------------------------------------------------------------------

n.ahead <- 12

y <- ts(append(jj, rep(0, n.ahead)), start = 1960, freq = 4)



# ----------
rmspe <- rep(0, n.ahead)

x00 <- ks$xf[,,num]

P00 <- ks$Pf[,,num]

Q <- t(cQ) %*% cQ

R <- t(cR) %*% cR



# ----------
for(m in 1:n.ahead){
  xp <- Phi %*% x00
  
  Pp <- Phi %*% P00 %*% t(Phi) + Q
  
  sig <- A %*% Pp %*% t(A) + R
  
  K <- Pp %*% t(A) %*% (1 / sig)
  
  x00 <- xp
  
  P00 <- Pp - K %*% A %*% Pp
  
  y[num + m] <- A %*% xp
  
  rmspe[m] <- sqrt(sig)
}



# ----------
par(mfrow = c(1, 1))

plot(y, type = "o", main = "", ylab = "J&J QE/Share", ylim = c(5, 30), xlim = c(1975, 1984))

upp <- ts(y[(num + 1):(num + n.ahead)] + 2 * rmspe, start = 1981, freq = 4)

low <- ts(y[(num + 1):(num + n.ahead)] - 2 * rmspe, start = 1981, freq = 4)

xx <- c(time(low), rev(time(upp)))

yy <- c(low, rev(upp))


polygon(xx, yy, border = 8, col = gray(0.5, alpha = 0.3))

abline(v = 1981, lty = 3)

