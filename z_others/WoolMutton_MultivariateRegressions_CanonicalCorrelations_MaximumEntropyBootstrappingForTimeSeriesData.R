# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "plm", "systemfit", "tseries")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  WoolMutton
#  - Estimation of joint production function for wool and mutton by capital anbd labor inputs based on the time series adta in Vinod
#  - Incorporating technological change by including cap * labor interaction
# ------------------------------------------------------------------------------
labor <- c(17703, 18552, 18814, 13490, 13390, 12710, 13100, 13350, 14280, 14710, 13290, 12560)
cap <- c(193649, 227548, 206165, 261888, 260120, 158990, 262540, 286690, 307320, 309180, 302420, 304810)
wool <- c(228091, 233309, 232258, 235807, 241284, 242177, 239101, 243713, 259939, 166563, 261249, 248538)
mutton <- c(11416, 14304, 16321, 16255, 16553, 16328, 15292, 14494, 15528, 16239, 17536, 17171)

labor <- ts(labor, frequency = 1, start=c(1951,1))
cap <- ts(cap, frequency = 1, start=c(1951,1))
wool <- ts(wool, frequency = 1, start=c(1951,1))
mutton <- ts(mutton, frequency = 1, start=c(1951,1))


# rescale
nn <- 10000
labor <- labor/nn;  cap <- cap/nn;  wool <- wool/nn;  mutton <- mutton/nn;


graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,2))
ts.plot(wool, main = "US Production of Wool")
ts.plot(mutton, main = "US Production of Mutton")
ts.plot(cap, main = "Capital input in joint production")
ts.plot(labor, main = "Labor input in joint production")



# ------------------------------------------------------------------------------
# Multivariate Regression:  canonical correlations
#
#  - Y * bet = rho * X * alph + e
#     - alph:  a vector of coefs on the X (endogenous variables)
#     - bet:  a vector of coefs on the Y (exogenous variables)
#     - rho:  square root of the largest caonical correlation (eigenvalue)
#  - Hoelling has proved that bet and alph vectors (weights) associated with the largest canonical correlation (eigenvalue) are optimal (best-fitting) under some conditions.
# ------------------------------------------------------------------------------
# canonical correlation between endogenous variables(Y: Wool, Mutton) and exogenous variables(X: Labor, Capital)
# X first then Y (counterintuitive to regressin y on x but this is the way it is in R)
cc <- cancor(cbind(cap, labor), cbind(wool, mutton))
cc


# alph:  a vector of coefs on the X (exogenous variables: cap, labor)
# bet:  a vector of coefs on the Y (endoenous variables: wool, mutton)
# rho:  square root of the largest caonical correlation (eigenvalue)
# ny = 2 and ny = 2, minxy = 2 --> we get 2 sets of coefs on the X side and Y side and 2 sets of estiamtes of rho
alph <- cc$xcoef[,1]
bet <- cc$ycoef[,1]

# if sign of weights on sr is negative, change all signs
if(bet[1] < 0){ bet <- -bet;  alph <- -alph }
rho <- sqrt(cc$cor[1])


# fitted relation is:
rho
round(c(bet, rho*alph), 5)


# check that we got best fitting linear combination  --> average error is zero
coefs <- c(bet, rho*alph)
dewool <- wool - mean(wool)
demutt <- mutton - mean(mutton)
decap <- cap - mean(cap)
delab <- labor - mean(labor)

cbind(coefs[1] * dewool + coefs[2] * demutt, coefs[3] * decap + coefs[4] * delab)
mean(coefs[1] * dewool + coefs[2] * demutt - coefs[3] * decap - coefs[4] * delab)


# ------------------------------------------------------------------------------
# bootstrap confidence intervals for time series data
#  - meboot:  Generate maximum entropy bootstrapped time series ensemble
# ------------------------------------------------------------------------------
library(meboot)

n999 <- 9999
nx <- 2
ny <- 2
nall <- nx + ny

cccoef <- matrix(rep(NA, nall * n999), nrow = n999, ncol = nall)

set.seed(234)

# first create n999 time series for each
wool2 <- meboot(x = wool, reps = n999, trim = 0.10, elaps = F)
mutton2 <- meboot(x = mutton, reps = n999, trim = 0.10, elaps = F)
cap2 <- meboot(x = cap, reps = n999, trim = 0.10, elaps = F)
labor2 <- meboot(x = labor, reps = n999, trim = 0.10, elaps = F)


# use meboot resamples
for(i in 1:n999){
  capz <- cap2$ens[,i]
  laborz <- labor2$ens[,i]
  woolz <- wool2$ens[,i]
  muttonz <- mutton2$ens[,i]
  
  cc <- cancor(cbind(capz, laborz), cbind(woolz, muttonz))
  
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
nam <- c(colnames(cbind(wool, mutton)), colnames(cbind(cap, labor)))
print(cbind(nam, lolim, round(coefs,4), uplim), q = F)


# The results suggest that the labor input has a negative impact on the output of wool and mutton
# However, the analysis of confidence intervals clarifies that the labor coef is not statistically significant
# The negative effect of labor in aggregate data may have to do with labor-saving technological changes.

# A deeper study of joint production would need better data and better specification which would allow for inventories, time lags, the effect of weather and incomes on the demand for wool,
# the effect of prices of competing meats such as fish, chicken, pork and beef.


# ------------------------------------------------------------------------------
# Joint production of Wool and Mutton 
# Cobb-Douglas specification with log s this time.
# ------------------------------------------------------------------------------
labor <- c(17703, 18552, 18814, 13490, 13390, 12710, 13100, 13350, 14280, 14710, 13290, 12560)
cap <- c(193649, 227548, 206165, 261888, 260120, 158990, 262540, 286690, 307320, 309180, 302420, 304810)
wool <- c(228091, 233309, 232258, 235807, 241284, 242177, 239101, 243713, 259939, 166563, 261249, 248538)
mutton <- c(11416, 14304, 16321, 16255, 16553, 16328, 15292, 14494, 15528, 16239, 17536, 17171)


# logs needed for the Cobb-Douglas model
labor <- log(labor)
cap <- log(cap)
wool <- log(wool)
mutton <- log(mutton)

labor <- ts(labor, frequency = 1, start=c(1951,1))
cap <- ts(cap, frequency = 1, start=c(1951,1))
wool <- ts(wool, frequency = 1, start=c(1951,1))
mutton <- ts(mutton, frequency = 1, start=c(1951,1))


# Index of technological change
tim <- 51:62

graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,2))
ts.plot(wool, main = "US Production of Wool")
ts.plot(mutton, main = "US Production of Mutton")
ts.plot(cap, main = "Capital input in joint production")
ts.plot(labor, main = "Labor input in joint production")


# Canonical Correlation including cap * labor 
cc <- cancor(cbind(cap, labor, (cap*labor)), cbind(wool, mutton))
cc

alph <- cc$xcoef[,1]
bet <- cc$ycoef[,1]
if(bet[1] < 0){ bet <- -bet;  alph <- -alph }
rho <- sqrt(cc$cor[1])


# fitted relation is:
rho
round(c(bet, rho*alph), 5)


# Maximum Entropy bootstrapping 
n999 <- 9999
nx <- 2
ny <- 2
nall <- nx + ny

cccoef <- matrix(rep(NA, nall * n999), nrow = n999, ncol = nall)

set.seed(234)
wool2 <- meboot(x = wool, reps = n999, trim = 0.10, elaps = F)
mutton2 <- meboot(x = mutton, reps = n999, trim = 0.10, elaps = F)
cap2 <- meboot(x = cap, reps = n999, trim = 0.10, elaps = F)
labor2 <- meboot(x = labor, reps = n999, trim = 0.10, elaps = F)
# use meboot resamples
for(i in 1:n999){
  capz <- cap2$ens[,i]
  laborz <- labor2$ens[,i]
  woolz <- wool2$ens[,i]
  muttonz <- mutton2$ens[,i]
  
  cc <- cancor(cbind(capz, laborz), cbind(woolz, muttonz))
  
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
nam <- c(colnames(cbind(wool, mutton)), colnames(cbind(cap, labor)))
print(cbind(nam, lolim, round(coefs,4), uplim), q = F)


# Cobb-Douglas intervals similar to those without logs.
# Extra regressor for technology makes coeff for cap and labor both negative