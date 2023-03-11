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
# Switzerland SMI:  basic analysis
# ------------------------------------------------------------------------------

smi <- EuStockMarkets[,"SMI"]

Rsmi <- diff(log(smi))



# ----------
par(mfrow=c(1,2), mar = c(2,2,2,2))
plot(smi, type = "l", main = "original SMI")
plot(log(smi), type = "l", main = "logged SMI")



# ----------
par(mfrow=c(1,1), mar = c(2,2,2,2))
plot(Rsmi, type = "l", main = "1st difference of logged SMI")



# ----------
# kurtosis is 5.73
psych::describe(Rsmi)



# ----------
# Histogram
par(mfrow=c(1,1), mar = c(2,2,2,2))
MASS::truehist(Rsmi, xlab = "1st difference of logged SMI", col = gray(.7))



# ------------------------------------------------------------------------------
# Switzerland SMI:  fitting automatically continuous real line distribution
# ------------------------------------------------------------------------------
 
# Normal distribution model is fitted first to start the process
m1 <- gamlssML(Rsmi, family = NO)


t1 <- chooseDist(m1, type = "realline", trace = TRUE)


t1



# ----------
# ordering of a specified column according to the relevant GAIC
getOrder(t1, 1)[1:10]
getOrder(t1, 2)[1:10]
getOrder(t1, 3)[1:10]



# ----------
# plot the best JSUo model and TF model
histDist(Rftse, family = "JSUo", nbins = 30, line.wd = 2.5)
histDist(Rftse, family = "TF", nbins = 30, line.wd = 2.5)



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
# Switzerland SMI:  Residual diagnostics
# ------------------------------------------------------------------------------

plot(mf, ts = TRUE)



# -->
# Note that kurtosis is almost 3.0



# ----------
wp(mf)

wp(mf, ylim.all = 1.0)

