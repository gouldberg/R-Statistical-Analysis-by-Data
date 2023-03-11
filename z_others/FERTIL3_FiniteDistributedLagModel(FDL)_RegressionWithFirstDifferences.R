# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "dynlm", "broom")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  FERTIL3
#
# Effects of Personal Exemption on Fertility Rates
# gfr:  the general fertility rate
# pe:  the personal tax exemption
# ww2:  dummy variables for the 2nd world war
# pill:  the availability of the birth control pill
#
# Finite Distributed Lag Model (FDL):  allow past values of regressor to affect the dependent variables
# Long-Run Propensity (LPR)
# ------------------------------------------------------------------------------
data <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/fertil3.dta")
dim(data)
glance(data)
str(data)
describe(data)


# define yearly time series beginning in 1913
tsdata <- ts(data, start = 1913)
tsdata


# gfr, pe + ww2 and pill period
plot(data$gfr, type = "l", lwd = 3, ylim = c(0,250), xaxt="n", xlab = "", ylab="", main = "GFR and PE", las = 1)
par(new=T);  plot(data$pe, type = "l", lwd = 3, lty = 1, col = "blue", ylim = c(0,250), xaxt="n", xlab = "", ylab = "", yaxt="n")
axis(side = 1, at = 1:nrow(data), labels = seq(1913,1984,by=1))
abline(v = min(which(data$ww2 == 1)), lty = 2, col = "gray")
abline(v = max(which(data$ww2 == 1)), lty = 2, col = "gray")
abline(v = min(which(data$pill == 1)), lty = 2, col = "gray")
abline(v = max(which(data$pill == 1)), lty = 2, col = "gray")

par(mfrow=c(2,1))
acf(data$gfr)
acf(data$pe)


# Linear regression of model with lags:  Finite Distributed Lag Models (FDL)
# L(x):  variable x lagged by one time unit
# L(x, k):  variable x, lagged by k times unit
mod <- dynlm(gfr ~ pe + L(pe) + L(pe,2) + ww2 + pill, data = tsdata)
lmtest::coeftest(mod)


# F test:  H0 is that all pe coefficients are = 0
# But "pe" are jointly significantly different from zero at a significant level of alpha = 5% with a p value of 0.012
linearHypothesis(mod, matchCoefs(mod, "pe"))


# But all "pe" coefficients are insignificantly different from zero according to the respective t tests.
# --> multicolliearlity problem
lmtest::coeftest(mod)
vif(mod)


# LPR (Long-Run Propensity):  measures the cumulative effect of a change in the independent variable z on the dependent variable y over time
# and is simply equal to the sum of the respective parameters
b <- coef(mod)
b["pe"] + b["L(pe)"] + b["L(pe, 2)"]


# F test:  H0 is that Long-Run Propensity (LPR) of FDL models  = 0
# Rejected, indicating that LPR is not zero
linearHypothesis(mod, "pe + L(pe) + L(pe, 2) = 0")



# ------------------------------------------------------------------------------
# Regression with Differences
# ------------------------------------------------------------------------------

# Linear regression of model with first differences
mod2 <- dynlm(d(gfr) ~ d(pe), data = tsdata)
lmtest::coeftest(mod2)


# Linear regression of model with lagged differences  (L(d(pe,2)):  the first difference, lagged by 2 time units)
# While the first difference of the tax exemptions has no significant effect, its second lag has a significant positive coef,
# consistent with fertility reacting two years after a change of the tax code.
mod3 <- dynlm(d(gfr) ~ d(pe) + L(d(pe)), data = tsdata)
mod4 <- dynlm(d(gfr) ~ d(pe) + L(d(pe, 2)), data = tsdata)
mod5 <- dynlm(d(gfr) ~ d(pe) + L(d(pe)) + L(d(pe, 2)), data = tsdata)
lmtest::coeftest(mod3)
lmtest::coeftest(mod4)
lmtest::coeftest(mod5)


stargazer(mod2, mod3, mod4, mod5, type = "text")

