# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/phillips5_aus")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  phillips5_aus
# ------------------------------------------------------------------------------
data("phillips5_aus", package = "POE5Rdata")

data <- phillips5_aus

glimpse(data)

str(data)

dim(data)


# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,3,4)], start = c(1987, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# Nonlinear Least Squares Estimation
#
# errors in AR(1) model has autocorrelation coefs
#  --> errors can be transformed into a model having uncorrelated errors, but in nonlinear in the coefs. (rho * beta0)
# inf(t) = alpha * (1 - rho) + rho * inf(t-1) + beta0 * diff(u(t)) - rho * beta0 * diff(u(t-1)) + v(t)
# ------------------------------------------------------------------------------

data.ts.tab <- cbind(data.ts[,"inf"], data.ts[,"u"],
                     stats::lag(data.ts[,"inf"], -1),
                     diff(data.ts[,"u"], lag = 1),
                     stats::lag(diff(data.ts[,2], lag = 1), -1))

dfr <- data.frame(data.ts.tab)

names(dfr) <- c("inf", "u", "Linf", "Du", "LDu")



# ----------
# Nonlinear Least Squares Estimation
# start, a list of initial guess values of the parameters
phill.nls <- nls(inf ~ alpha * (1 - rho) + rho * Linf + b0 * Du - rho * b0 * LDu, data = dfr, start = list(rho = 0.5,  alpha = 0.5,  b0 = -0.5))

summary(phill.nls)



# -->
# Standard errors of nonlinear model for Du is 0.11745 < s1
# indicating that nonlinear estimation is a better choice than HAC standard errors
s0  # OLS standard errors
s1  # HAC errors of linear model
summary(phill.nls)  # Standard erros of nonlinar model


# NLS model provides an estimate of the autocorrelation coefficient, rho = 0.512






# Calculate Delay multipliers, Interim multipliers and total multiplier
multipliers <- function(model, M){
  mdel <- numeric(M)  # creates vector of delay multipliers
  mint <- numeric(M)  # creates vector of interim multipliers
  delta <- coef(model)[[1]]
  lambda <- coef(model)[[2]]
  b0 <- coef(model)[[3]]
  for(s in 0:(M-1)){ mdel[s+1] <- b0 * lambda^s }
  for(s in 0:(M-1)){ mint[s+1] <- b0 * (1 - lambda^(s+1)) / (1 - lambda) }
  mtot <- b0 / ( 1 - lambda)
  lst <- list(mdel, mint, mtot)
}

mList <- multipliers(modC, 5)
tab <- data.frame(mList[[1]], mList[[2]], mList[[3]])
names(tab) <- c("Delay Multipliers", "Interim Multipliers", "Total Multiplier")
kable(tab, digits=3, align="c", caption = "Multipliers in Infinte Lag Model")


