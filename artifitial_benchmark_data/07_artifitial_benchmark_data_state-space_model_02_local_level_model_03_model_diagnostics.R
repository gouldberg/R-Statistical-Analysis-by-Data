rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Artifitial benchmark data
# ------------------------------------------------------------------------------

# generate benchmark data


set.seed(23)


library(dlm)



# ----------
# set model
W <- 1

V <- 2

m0 <- 10

C0 <- 9


# State Equation
f <- function(x, t) 1 / 2 * x + 25 * x / (1 + x^2) + 8 * cos(1.2 * t)


# Observation
h <- function(x) x^2 / 20


t_max <- 100


x_true <- rep(NA_real_, times = t_max + 1)

y <- rep(NA_real_, times = t_max + 1)


x_true[1] <- m0

for(it in (1:t_max) + 1){
  x_true[it] <- f(x_true[it - 1], it) + rnorm(n = 1, sd = sqrt(W))
  
  y[it] <- h(x_true[it]) + rnorm(n = 1, sd = sqrt(V))
}


x_true <- x_true[-1]

y <- y[-1]



# ----------

dat <- data.frame(x_true = x_true, y = y)

ts.plot(dat, ylab = "y", type = "l", lty = c(1,1), col = c("black", "gray"))


y <- as.ts(y)



# ------------------------------------------------------------------------------
# support functions for model diagnositcs
# ------------------------------------------------------------------------------

sub.LogLik <- function(tsNu, tsF, nStateVar){
  # purpose: get log likelihood.
  #          See Section 8.4
  # args:    tsNu:      (ts object)      time series of predicion error
  #          tsF:       (ts object)      time series of prediction error variance
  #          nStateVar: (numeric scalar) number of state variables
  # return:  (numeric scalar) log likelihood
  
  # trap: the length should be same
  stopifnot(length(tsF) == length(tsNu))
  
  # remove the first (nStateVar) timepoints
  agNu <- tsNu[-(1:nStateVar)]
  agF  <- tsF[-(1:nStateVar)]
  
  - 0.5 * length(tsNu) * log(2*pi) - 0.5 * sum( log(agF) + (agNu^2 / agF) )
}

sub.AIC <- function(gLogLik, nStateVar, nHyperParam){
  # purpose: get AIC
  #          See Section 2.1.
  # args:    gLogLik:     (numeric scalar) log likelihood
  #          nStateVar:   (integer scalar) number of state variables
  #          nHyperParam: (integer scalar) number of hyper params
  # return:  (numeric scalar) AIC
  
  - 2 * gLogLik + 2 * ( nStateVar + nHyperParam )
  
}

sub.BoxLjung <- function(agStdRes, nStateVar, nHyperParam, nLag=15){
  # purpose: Independence test for standardized residuals
  #          See Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  #          nHyperParam: (integer scalar) number of hyper params
  #          nLag:        (integer scalar) max lag
  # return:  (list)
  #            stat:     (numeric scalar) Q(nLag)
  #            critical: (numeric scalar) 5% critical value
  # notes:   Though the C&K book suggests all residuals can be used,
  #          their ssfpack program removes the first (nStateVar)
  #          residuals.
  
  # call Box.test
  gStat <- Box.test(
    agStdRes[-(1:nStateVar)], # remove the first (nStateVar) timepoints
    lag     = nLag,
    type    = "Ljung-Box"
  )$statistic
  
  # get critical value
  gCritical <- qchisq(0.95, nLag-nHyperParam+1)
  
  list(
    stat     = gStat,
    critical = gCritical
  )
}

sub.ResidualAcf <- function(agStdRes, nStateVar, nLag=15){
  # purpose: autocorrelation of standardized residuals
  #          See Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  #          nLag:        (integer scalar) max lag
  # return:  (list)
  #            acf:      (numeric vector) acf (1 ... nLag)
  #            critical: (numeric scalar) 95% confidence limit
  # notes:   Though the C&K book suggests all residuals can be used,
  #          their ssfpack program removes the first (nStateVar)
  #          residuals.
  
  list(
    acf = acf(agStdRes[-(1:nStateVar)], plot=FALSE)$acf[-1],
    critical = 2 / sqrt(length(agStdRes))
  )
  
}

sub.Homoscedasticity <- function(agStdRes, nStateVar){
  # purpose: Homoscedasticity test of standardized residuals
  #          See the C&K Book, Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  # return:  (list)
  #            size:  (integer scalar) block size
  #            stat:  (numeric scalar) H(block size)
  #            upper: (numeric scalar) 5% critical value (upper)
  #            lower: (numeric scalar) 5% critical value (lower)
  
  # get leength
  n <- length(agStdRes)
  
  # defint blocksize
  nBlockSize <- round((n - nStateVar)/3)
  gValue     <- sum(agStdRes[(n-nBlockSize+1):n]^2) /
    sum(agStdRes[(nStateVar+1):(nStateVar+nBlockSize)]^2)
  
  list(
    size  = nBlockSize,
    stat  = gValue,
    upper = qf(0.975, nBlockSize, nBlockSize),
    lower = qf(0.025, nBlockSize, nBlockSize)
  )
}

sub.JarqueBera <- function(agStdRes, nStateVar){
  # purpose: Normality test of standardized residuals
  #          See Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  # return:  (list)
  #            stat:     (numeric vector) statistic
  #            critical: (numeric vector) 5% critical value (=5.99)
  # notes:   Though the C&K book suggests all residuals can be used,
  #          their ssfpack program removes the first (nStateVar)
  #          residuals.
  
  require(tseries)
  
  list(
    stat     = jarque.bera.test(agStdRes[-(1:nStateVar)])$statistic,
    critical = qchisq(0.95,2)
  )
}

sub.ShowDiagnostics <- function(
  agStdRes, nStateVar, nHyperParam, nMaxLag, anACLag
){
  # purpose: Show Diagnostics Table
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  #          nHyperParam: (integer scalar) number of hyper params
  #          nMaxLag:     (integer scalar) max lag (for BoxLjung)
  #          anACLag:     (integer vector) two number of lag (for ACF)
  # return:  NULL
  
  # get various statistics
  lBoxLjung <- sub.BoxLjung(agStdRes, nStateVar, nHyperParam, nMaxLag)
  lACF      <- sub.ResidualAcf(agStdRes, nStateVar, nMaxLag)
  lHomo     <- sub.Homoscedasticity(agStdRes, nStateVar)
  lJB       <- sub.JarqueBera(agStdRes, nStateVar)
  
  # gHomoStat: H statistics, or its reciprocal
  gHomoStat <- ifelse(lHomo$stat > 1, lHomo$stat, 1/lHomo$stat)
  
  asTemplate <- c(
    "------------------------------------------------------",
    "                   stat     value  critical satisfied",
    "------------------------------------------------------",
    "independence      Q  (%2d)  %7.3f   %5.2f     %1s",  # BoxLJung, 4 args
    "                  r  (%2d)  %7.3f  +-%4.2f     %1s", # ACF,      4 args
    "                  r  (%2d)  %7.3f  +-%4.2f     %1s", # ACF,      4 args
    "homoscedasticity  %-3s(%2d)  %7.3f   %5.2f     %1s", # Homo,     5 args
    "normality         N        %7.3f   %5.2f     %1s",   # N,        3 args
    "------------------------------------------------------"
  )
  
  cat(
    sprintf(
      paste(asTemplate, collapse="\n"),
      # BoxLjung, 4 args
      nMaxLag,
      lBoxLjung$stat,
      lBoxLjung$critical,
      ifelse(lBoxLjung$stat < lBoxLjung$critical, "+", "-"),
      # ACF, 4 args
      anACLag[1],
      lACF$acf[anACLag[1]],
      lACF$critical,
      ifelse(abs(lACF$acf[anACLag[1]]) < lACF$critical, "+", "-"),
      # ACF, 4 args
      anACLag[2],
      lACF$acf[anACLag[2]],
      lACF$critical,
      ifelse(abs(lACF$acf[anACLag[2]]) < lACF$critical, "+", "-"),
      # Homo, 5 args
      ifelse(lHomo$stat > 1, "H", "1/H"),
      lHomo$size,
      gHomoStat,
      lHomo$upper,
      ifelse(gHomoStat < lHomo$upper, "+", "-"),
      # N, 3 args
      lJB$stat,
      lJB$critical,
      ifelse(lJB$stat < lJB$critical, "+", "-")
    )
  )
  cat("\n")
  
}



# ------------------------------------------------------------------------------
# model diagnositcs:  qqnorm for residuals
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

qqnorm(residuals(oFiltered.DLM, sd = FALSE))
qqline(residuals(oFiltered.DLM, sd = FALSE))

qqnorm(residuals(oFiltered2.DLM, sd = FALSE))
qqline(residuals(oFiltered2.DLM, sd = FALSE))

qqnorm(residuals(oFiltered3.DLM, sd = FALSE))
qqline(residuals(oFiltered3.DLM, sd = FALSE))




# ------------------------------------------------------------------------------
# model diagnositcs:  Independence / Homoscedasticity / Normality
# ------------------------------------------------------------------------------

# try oFiltered3

lPredErr.DLM <- residuals(oFiltered3.DLM, type = "raw", sd = TRUE)

agStdPredErr.DLM <- residuals(oFiltered3.DLM, type = "standardized", sd = FALSE)



# ----------
# nMaxLag:  max lag for BoxLjung
# anACLag:  two number of lag (for ACF)

sub.ShowDiagnostics(agStdPredErr.DLM, nStateVar, nHyperParam, nMaxLag = 10, anACLag = c(1,12))



# ------------------------------------------------------------------------------
# Goodness of Fit
# ------------------------------------------------------------------------------


# Log-Likelihood
( gLLbyFun.DLM <- - oMLE.DLM$value - 0.5 * n * log(2 * pi) )



# AIC
sub.AIC(gLLbyFun.DLM, nStateVar, nHyperParam)

sub.AIC(gLLbyFun.DLM, nStateVar, nHyperParam) / n
