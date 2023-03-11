# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "broom", "knitr", "tseries", "vars", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Simulate 2 Variables VAR(1)
#  - y1(t) <- c1 + phi11 * y1(t-1) + phi12 * y2(t-1) + e1(t)
#  - y2(t) <- c2 + phi21 * y1(t-2) + phi22 * y2(t-1) + e2(t)
#  - e(t) ~ W.N(cov.mat):  cov.mat = c(sigma1^2, rho * sigma1 * sigma2, rho * sigma1 * sigma2, sigma2^2)
# ------------------------------------------------------------------------------
library(mvtnorm)

# set parameters
n <- 1000

c1 <- -1;  phi11 <- 0.6;  phi12 <- 0.3;
c2 <- 1;  phi21 <- 0.1;  phi22 <- 0.8;
sigma1 <- 2;  sigma2 <- 1;  rho <- 0.6;

c1 <- 2;  phi11 <- 0.5;  phi12 <- 0.4;
c2 <- -3;  phi21 <- 0.6;  phi22 <- 0.3;
sigma1 <- 2;  sigma2 <- 1;  rho <- 0.6;


# generate bivariate Gaussian white noise
cov.mat <- matrix(c(sigma1^2, rho * sigma1 * sigma2, rho * sigma1 * sigma2, sigma2^2), nrow=2, ncol=2)

w <- rmvnorm(n, sigma = cov.mat)
cov(w)

wy1 <- w[,1]
wy2 <- w[,2]


# show the 2 white noise time series
ts.plot(w, col = c(1,2))
MTSplot(w)


# check the correlation between e1(t) and e2(t)
# If the correlation between errors, Impulse Response Function will be different between NON-ORTHOGONAL and ORTHOGONAL
cor(wy1, wy2)
rho * sigma1 * sigma2 / sqrt(sigma1^2 * sigma2^2)


# Check that the cross-correlations of simulated bivariate Gaussian white noise approximately zero for all non-zero lags
par(mfrow=c(1,1))
ccf(wy1, wy2, main = "")

# Also ccm can check cross-correlations and p-value plot of individual ccm
cov.mat
ccm(w, lags = 10)

# Ljung-Box Statistics for multivariate time series
mq(w, lag = 10)


# generate bivariate VAR(1) process
y2 <- y1 <- rep(0, n)
for(i in 2:n){
  y1[i] <- c1 + phi11 * y1[i-1] + phi12 * y2[i-1] + wy1[i]
  y2[i] <- c2 + phi21 * y1[i-1] + phi22 * y2[i-1] + wy2[i]
}


y1 <- ts(y1, start = c(1900,1), frequency = 4)
y2 <- ts(y2, start = c(1900,1), frequency = 4)


graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
ts.plot(y1, y2, type = "l", lty = c(1,2), col = c(1,2))
ts.plot(diff(y1), diff(y2), type = "l", lty = c(1,2), col = c(1,2))


# -------------
# VARMAsim can generate simulation data too
m1 <- VARMAsim(n, arlags = c(1), phi = matrix(c(phi11, phi12, phi21, phi22), nrow=2, ncol=2), sigma = cov.mat)
MTSplot(m1$series)
MTSplot(m1$noises)
ccm(m1$series)
ccm(m1$noises)


# ------------------------------------------------------------------------------
# Order of Integration
# ------------------------------------------------------------------------------
forecast::ndiffs(y1)
forecast::ndiffs(y2)


# ------------------------------------------------------------------------------
# Check the coefs of AR model
# ------------------------------------------------------------------------------
# As expected, the parameter estimates are close to the underlying model values
yar <- ar(cbind(y1,y2))
yar


# ------------------------------------------------------------------------------
# Correlogram to study the stationarity properties of a time series
#  - Nonstationary series often display serial correlations for many lags
# ------------------------------------------------------------------------------
library(forecast)

graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(3,2))
Acf(y1, main = "", xlab = "(a) Acf y1", ylab = "")
Acf(y2, main = "", xlab = "(a) Acf y2", ylab = "")
Acf(diff(y1), main = "", xlab = "(b) First Difference in y1", ylab = "")
Acf(diff(y2), main = "", xlab = "(b) First Difference in y2", ylab = "")
Pacf(y1, main = "", xlab = "(c) Pacf y1", ylab = "")
Pacf(y2, main = "", xlab = "(c) Pacf y2", ylab = "")


# ------------------------------------------------------------------------------
# Unit Root Tests for Stationarity
# ------------------------------------------------------------------------------
# ADF test with Intercept and No Trend
adf.test(y1)
adf.test(y2)


# The test indicates I(1) ?
adf.test(diff(y1))
adf.test(diff(y2))


library(fUnitRoots)
library(urca)


# ADF test
# type="c":  random walk model with drift
# type="nc":  random walk model
# type="tc":  random walk model with drift and time trend
# If H0 is rejected, indicates that the process is NOT UNIT ROOT PROCESS
unitrootTest(y1, type = "nc", lags=1)
unitrootTest(y2, type = "nc", lags=1)


# Phillips-Perron Test
# model = "constant":  test based on model with constant term
# model = "trend":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
# If H0 is rejected, indicates that the process is NOT UNIT ROOT PROCESS
summary(ur.pp(y1, type="Z-tau", model="constant", lags="long"))
summary(ur.pp(y2, type="Z-tau", model="constant", lags="long"))


# KPSS Test HAS CONSISTENT RESULTS
# model = "mu":  test based on model with constant term
# model = "tau":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
# If H0 is rejected, indicates that the process IS UNIT ROOT PROCESS
summary(ur.kpss(y1, type="mu", lags="long"))
summary(ur.kpss(y2, type="mu", lags="long"))


# ------------------------------------------------------------------------------
# Cointegration Test
#  - test whether the residuals from regressin one series on the other one are stationary  (Dickey Fuller stationarity test on residuals)
#  - Phillips-Ouliaris test (po.test)
# ------------------------------------------------------------------------------
# If is rejected H0 that residuals have unit roots, therefore the series are conintegrated
cointy1y2.dyn <- dynlm(y1 ~ y2, data = cbind(y1, y2))
kable(tidy(cointy1y2.dyn), digis = 3, caption = "The Results of the Cointegration Equation")

ehat <- resid(cointy1y2.dyn)
adf.test(ehat)  # note that adf.test function uses a constant and trend  --> If H0 is rejected, indicating Cointegrated

output <- dynlm(d(ehat) ~ L(ehat))
kable(tidy(output), digits = 4, alighn = "c", caption = "Cointegration Test Between y1 and y2")

par(cex = 1.2, lwd = 1.6, mfrow=c(1,1))
plot(ehat)


# Phillips-Ouliaris test:  if H0 of no cointegration is rejected  -->  cointegrated
cy1y2 <- as.matrix(cbind(y1, y2), demean = FALSE)
tseries::po.test(cy1y2)


# ------------------------------------------------------------------------------
# Vector Error Correction (VEC) Model
#   - IF TWO TIME SERIES ARE COINTEGRATED
# ------------------------------------------------------------------------------
# With cointegrated series we can construct a VEC model to better understand the causal relationship between the two variables
vecy1 <- dynlm(d(y1) ~ L(ehat))
vecy2 <- dynlm(d(y2) ~ L(ehat))


# If the coefs on the error correction term L(ehat) is significant for y1, suggesting that changes in y1 do affect y2
tidy(vecy1)


# If the coefs on the error correction term L(ehat) is significant for y2, suggesting that changes in y2 do affect y1
tidy(vecy2)


# ------------------------------------------------------------------------------
# Vector Autoregression Model (VAR)
# ------------------------------------------------------------------------------
# Recursive Structure 1
varmat <- as.matrix(cbind(y1, y2))
# Recursive Structure 2
varmat2 <- as.matrix(cbind(y2, y1))


# determine optimal lag length according to AIC, HQ, SC, FPE(final prediction error) of an empirical VAR(p) process
vars::VARselect(varmat, lag.max = 10, type = "const")
MTS::VARorder(cbind(y1, y2))


# we choose p = 1 and type = "const"
varfit <- vars::VAR(varmat, p = 1, type = "const")
varfit2 <- vars::VAR(varmat2, p = 1, type = "const")
varfit_MTS <- MTS::VAR(cbind(y1, y2), p = 1)

summary(varfit)



# ------------------------------------------------------------------------------
# Model Simplification (Refinement)
# ------------------------------------------------------------------------------
# refVAR carry out model simplification of a fitted VAR model.
# The command uses a threshold to select target parameters for removal and computes information criteria of the simplified model for validation.
# The default threshold is 1.00
# The command also allows the user to specify zero parameters with a subcommand fixed.
varfit_MTS_ref <- refVAR(varfit_MTS, thres = 1.96)


varfit_MTS$aic;  varfit_MTS$bic;
varfit_MTS_ref$aic;  varfit_MTS_ref$bic;


# ------------------------------------------------------------------------------
# Model Checking
# ------------------------------------------------------------------------------
# fit and residuals, ACF residuals, PACF residuals
plot(varfit)


# Multivariate Pormanteau statistics, adjusted by degrees of freedom
# AR(p) and k is number of vectors (time-series)
p <- 1;  k <- 2;
mq(varfit_MTS$residuals, adj = p * k^2)


# MTSdiag checks 24 lags of cross-correlation matrix in default
# p-value plot of individual ccm
# plot of p-values of the Qk(m) statistics
# residual plots
MTSdiag(varfit_MTS, adj = p * k^2)


# check stability: stable model has all roots < 1
vars::roots(varfit)



# ------------------------------------------------------------------------------
# Granger causality testing
# ------------------------------------------------------------------------------
# If p-values are low in both types of causality tests (Granger and instantaneous), we reject noncausality, supporting possible causations
vars::causality(varfit, cause = "y1")

vars::causality(varfit, cause = "y2")



# ------------------------------------------------------------------------------
# Impulse Responses and Variance Decompositions
# ------------------------------------------------------------------------------
# Recursive Structure 1
impresp_nonortho_y2toy1 <- irf(varfit, impulse = "y2", response = "y1", n.ahead = 10, ortho = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
impresp_nonortho_y1toy2 <- irf(varfit, impulse = "y1", response = "y2", n.ahead = 10, ortho = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
impresp_ortho_y2toy1 <- irf(varfit, impulse = "y2", response = "y1", n.ahead = 10, ortho = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
impresp_ortho_y1toy2 <- irf(varfit, impulse = "y1", response = "y2", n.ahead = 10, ortho = TRUE, boot = TRUE, ci = 0.95, runs = 1000)


# Recursive Structure 2
impresp_nonortho_y2toy1_2 <- irf(varfit2, impulse = "y2", response = "y1", n.ahead = 10, ortho = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
impresp_nonortho_y1toy2_2 <- irf(varfit2, impulse = "y1", response = "y2", n.ahead = 10, ortho = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
impresp_ortho_y2toy1_2 <- irf(varfit2, impulse = "y2", response = "y1", n.ahead = 10, ortho = TRUE, boot = TRUE, ci = 0.95, runs = 1000)
impresp_ortho_y1toy2_2 <- irf(varfit2, impulse = "y1", response = "y2", n.ahead = 10, ortho = TRUE, boot = TRUE, ci = 0.95, runs = 1000)


# Plot Impulse Response Function
plot(impresp_nonortho_y2toy1, main = "impulse response from y2 to y1, non-orthogonal")
plot(impresp_nonortho_y1toy2, main = "impulse response from y1 to y2, non-orthogonal")
plot(impresp_ortho_y2toy1, main = "impulse response from y2 to y1, orthogonal")
plot(impresp_ortho_y2toy1, main = "impulse response from y1 to y2, orthogonal")


# NOTE THAT THE NON-ORTHOGONAL IMPULSE RESPONSE FUNCTION IS NOT DIFFERENT
plot(impresp_nonortho_y2toy1, main = "impulse response from y2 to y1, non-orthogonal")
plot(impresp_nonortho_y2toy1_2, main = "impulse response from y2 to y1, non-orthogonal")

plot(impresp_nonortho_y1toy2, main = "impulse response from y1 to y2, non-orthogonal")
plot(impresp_nonortho_y1toy2_2, main = "impulse response from y1 to y2, non-orthogonal")


# BUT THE ORTHOGONAL IMPULSE RESPONSE FUNCTION IS DIFFERENT !!!
plot(impresp_ortho_y2toy1, main = "impulse response from y2 to y1, orthogonal")
plot(impresp_ortho_y2toy1_2, main = "impulse response from y2 to y1, orthogonal")

plot(impresp_ortho_y2toy1, main = "impulse response from y1 to y2, orthogonal")
plot(impresp_ortho_y2toy1_2, main = "impulse response from y1 to y2, orthogonal")


# Forecast variance decomposition estimates the contribution of a shock in each variable to the reponse in both variables.
# almost 100 % of the variance of y1 is caused by y1 and the rest is caused by y2
plot(vars::fevd(varfit))

# NOTE that Forecast variance decomposition is DIFFERENT
plot(vars::fevd(varfit2))


# by MTS:  Original and Orthogonal innovations:  by column JP causes, UK causes, US causes
# AND Variance Decomposition:  by column JP ratio in JP/UK/US, UK ratio in JP/UK/US and US ratio in JP/UK/US
MTS::VARirf(varfit_MTS$Phi, varfit_MTS$Sigma, orth=F)
MTS::VARirf(varfit_MTS$Phi, varfit_MTS$Sigma)


# by MTS:  Forecast Error Vairance Decomposition
MTS::FEVdec(Phi = varfit_MTS$Phi, Theta = NULL, Sig = varfit_MTS$Sigma, lag = 10)



# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------
# Out-of-sample forecasting of VAR models and fan-chart
var.pred <- predict(varfit, n.ahead = 10, ci = 0.95)
var.pred2 <- predict(varfit2, n.ahead = 10, ci = 0.95)
plot(var.pred)
plot(var.pred2)


fanchart(var.pred)
fanchart(var.pred2)


# VARpred
VARpred(varfit_MTS, h = 10)


