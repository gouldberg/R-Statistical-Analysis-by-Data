setwd("//media//kswada//MyFiles//R//eustockmarkets")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EuStockMarkets
# ------------------------------------------------------------------------------
data("EuStockMarkets", package = "datasets")


str(EuStockMarkets)

head(EuStockMarkets)



# ------------------------------------------------------------------------------
# UK FTSE:  basic analysis
# ------------------------------------------------------------------------------

ftse <- EuStockMarkets[,"FTSE"]

Rftse <- diff(log(ftse))



# ----------
par(mfrow=c(2,1), mar = c(2,2,2,2))
plot(ftse, type = "l", main = "original FTSE")
plot(log(ftse), type = "l", main = "logged FTSE")



# ----------
par(mfrow=c(1,1), mar = c(2,2,2,2))
plot(Rftse, type = "l", main = "1st difference of logged FTSE")



# ----------
# kurtosis is 2.63
psych::describe(Rftse)



# ----------
# Histogram
par(mfrow=c(1,1), mar = c(2,2,2,2))
MASS::truehist(Rftse, xlab = "1st difference of logged FTSE", col = gray(.7))



# -->
# almost close to normal distribution ...



# ------------------------------------------------------------------------------
# UK FTSE:  fitting automatically continuous real line distribution
# ------------------------------------------------------------------------------
 
# Normal distribution model is fitted first to start the process
m1 <- gamlssML(Rftse, family = NO)


t1 <- chooseDist(m1, type = "realline", trace = TRUE)


t1



# ----------
# ordering of a specified column according to the relevant GAIC
getOrder(t1, 1)[1:10]
getOrder(t1, 2)[1:10]
getOrder(t1, 3)[1:10]



# ----------
# plot the best TF model and LO model
histDist(Rftse, family = "TF", nbins = 30, line.wd = 2.5)
histDist(Rftse, family = "LO", nbins = 30, line.wd = 2.5)



# ----------
mf <- update(m1, family = "TF")

summary(mf)



# ----------
# The value of the fitted parameters (after link function is applied)
# can also be obtained

fitted(mf, "mu")[1]
fitted(mf, "sigma")[1]
fitted(mf, "nu")[1]
# fitted(mf, "tau")[1]



# ------------------------------------------------------------------------------
# UK FTSE:  Residual diagnostics
# ------------------------------------------------------------------------------

plot(mf, ts = TRUE)



# -->
# Note that kurtosis is almost 3.0



# ----------
wp(mf)

wp(mf, ylim.all = 1.0)

