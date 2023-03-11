# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "plm", "systemfit", "tseries")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  LifeCycleSavings  (variables of Modigliani model)
#  - the data is from 1960 to 1970 containing a cross-section of 50 countries on five variables
#  - sr:  aggregate personal saving divided by disposable income
#  - pop15:  % of population under 15
#  - pop75:  % of population over 75
#  - dpi:  real per-capita disposable income
#  - ddpi:  % rate of change in per-capita disposable income
#
#  - Modigliani's famous life-cycle savings model explains "sr" by dpi, ddpi and the two demographic variables pop15 and pop75
#  - The data are averages over the decade 1960-1970 to remove the business cycle or other short-term fluctuations
# ------------------------------------------------------------------------------
data("LifeCycleSavings")
data <- LifeCycleSavings
str(data)


psych::pairs.panels(data)



# ------------------------------------------------------------------------------
# Multivariate Regression:  canonical correlations
#
#  - Y * bet = rho * X * alph + e
#     - alph:  a vector of coefs on the X (endogenous variables)
#     - bet:  a vector of coefs on the Y (exogenous variables)
#     - rho:  square root of the largest caonical correlation (eigenvalue)
#  - Hoelling has proved that bet and alph vectors (weights) associated with the largest canonical correlation (eigenvalue) are optimal (best-fitting) under some conditions.
# ------------------------------------------------------------------------------

reg1 <- lm(sr ~ dpi + ddpi + pop15 + pop75, data = data)
summary(reg1)


# canonical correlation between endogenous variables(Y: sr, dpi, ddpi) and exogenous variables(X: pop15, pop75)
# X first then Y (counterintuitive to regressin y on x but this is the way it is in R)
cc <- cancor(cbind(data$pop15, data$pop75), cbind(data$sr, data$dpi, data$ddpi))
cc


# alph:  a vector of coefs on the X (exogenous variables: pop15, pop75)
# bet:  a vector of coefs on the Y (endoenous variables: sr, dpi, ddpi)
# rho:  square root of the largest caonical correlation (eigenvalue)
# ny = 3 and ny = 2, minxy = 2 --> we get 2 sets of coefs on the X side and Y side and 2 sets of estiamtes of rho
alph <- cc$xcoef[,1]
bet <- cc$ycoef[,1]

# if sign of weights on sr is negative, change all signs
if(bet[1] < 0){ bet <- -bet;  alph <- -alph }
rho <- sqrt(cc$cor[1])


# fitted relation is:
rho
round(c(bet, rho*alph), 5)


# ------------------------------------------------------------------------------
# bootstrap confidence intervals
# ------------------------------------------------------------------------------
n999 <- 999
nx <- 2
ny <- 3
nall <- nx + ny

cccoef <- matrix(rep(NA, nall * n999), nrow = n999, ncol = nall)

attach(data)

set.seed(234)

for(i in 1:n999){
  sr2 <- sample(sr, replace = T)
  dpi2 <- sample(dpi, replace = T)
  pop152 <- sample(pop15, replace = T)
  pop752 <- sample(pop75, replace = T)
  ddpi2 <- sample(ddpi, replace = T)
  cc <- cancor(cbind(pop152, pop752), cbind(sr2, dpi2, ddpi2))
  
  alph <- cc$xcoef[,1]
  bet <- cc$ycoef[,1]
  rho <- sqrt(cc$cor[1])

  if(bet[1] < 0){ bet <- -bet;  alph <- -alph }
  cccoef[i,] <- c(bet, rho*alph)
}


for(j in 1:nall){
  x <- sort(cccoef[,j])
  cccoef[,j] <- x
}


print("Lower and upper Limit of a 95% confidence interval")
lolim <- round(cccoef[round((n999)*0.025),],4)
uplim <- round(cccoef[round((n999)*0.975),],4)
nam <- c(colnames(cbind(sr2, dpi2, ddpi2)), colnames(cbind(pop152, pop752)))
print(cbind(nam, lolim, uplim), q = F)


# --> Since all confidence intervals contain the zero, except the one with "sr", none of hte coefs are significant.
# This evidence based on canonical correlations optimal fit rejects Modigliani's life cycle model of consumer savings behavior

