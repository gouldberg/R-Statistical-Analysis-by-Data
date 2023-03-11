# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/usmacro")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  usmacro
#
# u:  U.S. unemployment rate over a period of more than six decades
# g:  U.S. growth rate
# ------------------------------------------------------------------------------
data("usmacro", package = "POE5Rdata")

data <- usmacro

glimpse(data)

str(data)


is.ts(usmacro)


# ----------
# This ts object includes u and g together !!!
us.ts <- ts(usmacro, start = c(1948, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# ARDL(p, q):  p lags of the dependent variable and q lags of an independent variable
# --> Choosing (p, q) by BIC (SC) = ln(SSE/N) + K * ln(N) / N
# ------------------------------------------------------------------------------
bics <- matrix(0, nrow = 9, ncol = 8)


# use dynlm()
for(p in 1:8){
  ari <- dynlm(u ~ L(u, 1:p), data = us.ts)
  SSE <- sum(residuals(ari)^2)
  N <- nobs(ari)
  K <- N - ari$df.residual
  bics[1,p] <- log(SSE/N) + K * log(N) / N
}


for(p in 1:8){
  for(q in 1:8){
    ari <- dynlm(u ~ L(u, 1:p) + L(g, 1:q), data = us.ts)
    SSE <- sum(residuals(ari)^2)
    N <- nobs(ari)
    K <- N - ari$df.residual
    bics[1+q,p] <- log(SSE/N) + K * log(N) / N
  }
}


tbl <- data.frame(cbind(0:8, bics))

names(tbl) <- c("q/p", 1:8)

kable(tbl, digits = 3, align = "c", caption = "Lag Order Selection for an AR Model")



# ----------
# The lowest BIC (SC) value indicates the model with only 2 lags of the dependent variable as the winner
mOpt <- which(bics == min(bics), arr.ind=TRUE)

cat("Optimal model: p ~ ", mOpt[[2]], ", q = ", mOpt[[1]] - 1)



# ------------------------------------------------------------------------------
# Check again by correlogram
# ------------------------------------------------------------------------------
Acf(us.ts[,"u"])

Acf(us.ts[,"g"])

