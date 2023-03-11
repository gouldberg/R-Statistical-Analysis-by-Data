# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/gdp5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  
#  - gdp5:  U.S. GDP over the period of 1984Q1 through 2016Q1
#  - usdata5:  monthly data on the variables inflation, federal funds rate, and bond rate in the U.S. over the period of August 1954 through December 2016
# ------------------------------------------------------------------------------
data("gdp5", package = "POE5Rdata")
data("usdata5", package = "POE5Rdata")


is.ts(gdp5)
is.ts(usdata5)


head(gdp5)
head(usdata5)



# ----------
# convert to ts object
# Note that gdp5 and usdata5 have different frequency (quarterly and monthly)
gdp.ts <- ts(gdp5$gdp, start = c(1984, 1), end = c(2016, 4), frequency = 4)
usa.ts <- ts(usdata5, start = c(1954, 8), end = c(2016, 12), frequency = 12)



# ----------
# create data.frame from ts object
usa.ts.df <- data.frame(b = usa.ts[,2], f = usa.ts[,3], inf = usa.ts[,4])



# ------------------------------------------------------------------------------
# Error Correction model:  short run effect + long run effect
#  - a relationship between cointegrated I(1) variables is a long run relationship, while a relationship between I(0) is a short run one.
#  - The short run error correction model combines in some sense short run and long run effects.
# 
# ARDL(1,1):  y(t) + delta = theta1  * y(t-1) + delta0 * x(t) + delta1 * x(t-1) + v(t)
#  -->  error correction model:  delta y(t) = -alpha * (y(t-1) - beta1 - beta2 * x(t-1) ) + delta0 * delta x(t) + v(t)
#        ARDL(1,1) with steady state(long run) relationship between y and x
# ------------------------------------------------------------------------------
b.ols <- dynlm(L(b) ~ L(f))

b1ini <- coef(b.ols)[[1]]

b2ini <- coef(b.ols)[[2]]

d.ols <- dynlm(b ~ L(b) + f + L(f))

aini <- 1 - coef(d.ols)[[2]]

d0ini <- coef(d.ols)[[3]]

d1ini <- coef(d.ols)[[4]]

Db <- diff(b)

Df <- diff(f)

Lb <- stats::lag(b, -1)

Lf <- stats::lag(f, -1)

LDf <- stats::lag(diff(f), -1)



# ----------
bfset <- data.frame(ts.union(cbind(b, f, Lb, Lf, Db, Df, LDf)))

formula <- Db ~ -a * (Lb - b1 - b2 * Lf) + d0 * Df + d1 * LDf



# ----------
# use nls()
bf.nls <- nls(formula, na.action=na.omit, data = bfset, start=list(a = aini, b1 = b1ini, b2 = b2ini, d0 = d0ini, d1 = d1ini))


kable(tidy(bf.nls), caption = "Parameter Estimates in the Error Correction Model")

