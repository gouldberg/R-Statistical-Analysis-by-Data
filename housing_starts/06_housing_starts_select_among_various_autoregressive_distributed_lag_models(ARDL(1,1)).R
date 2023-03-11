# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/housing_starts")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: housing starts  
# ------------------------------------------------------------------------------

starts <- c(1252, 1313, 1463, 1610, 1529, 1473, 1165, 1292, 1508, 1467,
            1434, 2052, 2357, 2045, 1338, 1160, 1538, 1987, 2020, 1745,
            1292, 1084, 1062, 1703, 1750, 1742, 1805, 1621, 1488, 1376,
            1193, 1014, 1200, 1288, 1457, 1354, 1477, 1474, 1617, 1641,
            1569, 1603, 1705, 1848, 1956, 2068)

sales <- c(485, 656, 718, 634, 519, 549, 646, 819, 817, 709, 545,
           436, 412, 623, 639, 688, 750, 671, 676, 650, 534, 509,
           610, 666, 670, 667, 757, 804, 886, 880, 877, 908, 973, 1086, 1203, 1283)


# ----------
# Note that sales data is leading 10 years
length(starts)
length(sales)



# ----------
# add NA to first 10 years to sales
sales2 <- c(rep(NA, 10), sales)
length(sales2)



# ----------
data <- data.frame(starts = starts, sales2 = sales2)

data.ts <- ts(data, start = c(1960), end = c(2005), frequency = 1)



# ----------
starts = ts(starts, start = c(1960, 1))
dmstarts = starts - mean(starts)

sales = ts(sales, start = c(1970, 1))
dmsales = sales - mean(sales)



# ------------------------------------------------------------------------------
# Select model from varisou ARDL(1,1) models
#   - Professor David Hendry of Oxford University and coauthors have collected an interesting list of 11 possible economic interpretaions
#     of the simple ARDL(1,1) mode.
#  --> See "Hands-On Intermediate Econometrics Using R", p.161 (Chapter3)
# ------------------------------------------------------------------------------

# Intercepts
nam <- c(
  "Autoregressive-Distributed Lag",
  "Static Regression",
  "Univariate Time Series",
  "Differenced Data",
  "Leading Indicator",
  "Partial Adjustment",
  "Common Factor (Autoregressive error)",
  "Finite Distributed Lag",
  "Dead Start",
  "Homogeneous Equilibrium Correction",
  "General Equilibrium Correction")



# ----------
# number of restrictions by model number
nrestr = c(0, 2, 2, 2, 2, 1, 3, 1, 1, 3, 0)



# ----------
# F = [ (ReRSS - UnRSS) / r ] / [ UnRSS / df ]
adjRsq <- RSS <- Fstat <- Fcrit <- rep(NA, 11)



# ----------
library(dyn)

intercept = 0

regM1 = dyn$lm(dmsales ~ dmstarts + lag(dmsales, -1) + lag(dmstarts, -1) -1)
regM2 = dyn$lm(dmsales ~ dmstarts - 1)
regM3 = dyn$lm(dmsales ~ lag(dmsales - 1) -1)
regM4 = dyn$lm(diff(dmsales) ~ diff(dmstarts) -1)
regM5 = dyn$lm(dmsales ~ lag(dmstarts, -1) -1)
regM6 = dyn$lm(dmsales ~ dmstarts + lag(dmsales, -1) - 1)
regM8 = dyn$lm(dmsales ~ dmstarts + lag(dmstarts, -1) - 1)
regM9 = dyn$lm(dmsales ~ lag(dmsales, -1) + lag(dmstarts, -1) -1)
regM10 = dyn$lm(diff(dmsales) ~ dmstarts + lag((dmsales - dmstarts), -1) -1)
# regM11 = dyn$lm(diff(dmsales) ~ dmstarts + lag((dmsales - bigk * dmstarts), -1) -1)
regM11 = regM1  # model M11 imposes no restriction, equivalent M1


# ----------
# regM7
dmsalesLag1 = dmsales[1:35]
dms = dmsales[2:36]  # dependent variable has 35 observations
dmst = dmstarts[12:46] # must start at 12 to get 35 observations right
dmstartsLag1 = dmstarts[11:45]  # lagged starts at 11

# check start value for bet1 and bet2
summary(lm(dms ~ dmst + dmsalesLag1 + dmstartsLag1))
# regM7 = nls(dms ~ bet1 * dmst + bet2 * dmsalesLag1 - (bet1 * bet2) * dmstartsLag1, start = list(bet1 = 0.0535, bet2 = 0.9476))
regM7 = nls(dms ~ bet1 * dmst + bet2 * dmsalesLag1 - (bet1 * bet2) * dmstartsLag1, start = list(bet1 = 0.292, bet2 = 1.064))



# ----------
# stats for regM1
su = summary(regM1)
adjRsq[1] = su$adj
RSS[1] = su$df[2] * ((su$sigma)^2)


# ----------
# stats for common
UnRSS = RSS[1]
refdf = su$df[2]
for(i in 2:10) Fcrit[i] = qf(0.95, df1 = nrestr[i], df2 = refdf)



# ----------
# stats for regM2 - 10, excepts regM7
for(i in c(2:6,8:10)){
  eval(parse(text = paste0("mod <- regM", i)))
  su = summary(mod)
  adjRsq[i] = su$adj
  RSS[i] = su$df[2] * ((su$sigma)^2)
  Fnum = (su$df[2] * ((su$sigma)^2) - UnRSS) / nrestr[i]
  Fstat[i] = Fnum / (UnRSS / refdf)
}



# ----------
# stats for regM7
su = summary(regM7)
ebet1 = su$coef[1]
ebet2 = su$coef[2]
fitt = ebet1 * dmst + ebet2 * dmsalesLag1 - (ebet1 * ebet2) * dmstartsLag1
cor1 = cor(dms, fitt)
Rsquare = cor1 ^2
adjRsq[7] = 1 - (1 - Rsquare) * (34 / (34 - 2))
RSS[7] = sum((dms - fitt)^2)
Fnum = (RSS[7] - UnRSS) / nrestr[7]
Fstat[7] = Fnum / (UnRSS / refdf)



# ----------
# stats for regM11
bigk = (regM1$coef[1] + regM1$coef[3]) / (1 - regM1$coef[3])
print(" Model M11 is same as the unrestricted model M1, with K = ")
as.numeric(bigk)

su = summary(regM11)
adjRsq[11] = su$adj
RSS[11] = su$df[2] * ((su$sigma)^2)



# ----------
print(" Model, Adjusted R-square, Residual Sum of Squares")
print(cbind(nam, round(adjRsq, 5), round(RSS, 0)), q = F)

# -->
# It appears that only the nonlinear M7 has a higher adjusted R^2 than the unrestricted model.


print(" Model, F statistic, Critical Value of F, Num df")
print(cbind(nam, round(Fstat, 6), round(Fcrit, 6), nrestr), q = F)

 
# -->
# It appears that restrictions do significantly worsen the fit for models: M2, M3, M5, M6, M8, M9, and M10



# -->
# ARDL(1,1): y(t) = beta1 * x(t) + beta21 * y(t-1) + beta22 * y(t-2) + ...+ beta31 * x(t-1) + beta32 * x(t-2) + ...+ errors


# regM7:  Common Factor and Autoregressive Error model:
# y(t) = beta1 * x(t) + u(t)
# u(t) = beta2 * u(t-1) + v(t)
# beta3 = - beta1 * beta2   (non-linear relationship)
# Unfortunately, this model often lacks an economic interpretation


# regM4:  Differenced Data model:
# d(y(t)) = beta1 * d(x(t)) + error
# beta2 = 1,   beta1 = - beta3

