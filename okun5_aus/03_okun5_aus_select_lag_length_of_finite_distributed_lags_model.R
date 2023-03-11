# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/okun5_aus")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  okun5_aus
# ------------------------------------------------------------------------------
data("okun5_aus", package = "POE5Rdata")

glimpse(okun5_aus)

str(okun5_aus)


# remove dateid01
data <- okun5_aus[, c(2,3)]



# ----------
# convert to ts object
is.ts(data)

data.ts <- ts(data, start = c(1978, 2), end = c(2016, 2), frequency = 4)



# ------------------------------------------------------------------------------
# Finite Distributed Lags Model (FDL(q)):  y(t) = alpha + beta0 * x(t) + beta1 * x(t-1) + ... + betaq * x(t-q) + e(t)
# FDL(q):  q lags of an independent variable
# --> Choosing q by BIC (SC) = ln(SSE/N) + K * ln(N) / N
# ------------------------------------------------------------------------------
bics <- c()


# Okun's Law: u(t) - u(t-1) = - gamma * (g(t) - g(N))
# The econometric model:  D(u(t)) = alpha + beta0 * g(t) + beta1 * g(t-1) + ... + betaq * g(t-1) + e(T)

for(q in 1:8){
  fdl <- dynlm(d(u) ~ L(g, 1:q), data = data.ts)
  SSE <- sum(residuals(fdl)^2)
  N <- nobs(fdl)
  K <- N - fdl$df.residual
  bics[q] <- log(SSE/N) + K * log(N) / N
}


bics


# ----------
# The lowest BIC (SC) value indicates the model with only 2 lags of the dependent variable as the winner
mOpt <- which(bics == min(bics), arr.ind=TRUE)

cat("Optimal model: q = ", mOpt[[1]])


