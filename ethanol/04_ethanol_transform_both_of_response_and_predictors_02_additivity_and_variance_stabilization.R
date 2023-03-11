setwd("//media//kswada//MyFiles//R//ethanol")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ethanol
# ------------------------------------------------------------------------------

data(ethanol, package="lattice")

str(ethanol)



# ------------------------------------------------------------------------------
# Additivity and Variance Stabilization (AVAS)
#  - Another transform both sides (TBS) model, quite similar to ACE.
#    We choose the fj to optimize the fit, but we also want constant variance for the response. 
#  - The purpose of the AVAS method is to obtain additivity and variance stabilization and not necessarily to produce the best possible fit.
# ------------------------------------------------------------------------------

library(acepack)


# ----------
# We start with the same 3 predictors.
# We must give it the X matrix explicitly.
# The function returns the components ty which contains theta(y) and tx which is a matrix whose columns contain the fj(xj)

x <- ethanol[c("C", "E")]

avasfit <- avas(x, ethanol$NOx)


avasfit$ty

avasfit$tx



# ----------
# We can get a sense of how well these transformations work by fitting a linear model that uses the transformed variables

summary(lm(avasfit$ty ~ avasfit$tx))



# ----------
# Now we examine the transforms on the response and the 3 predictors

par(mfrow=c(1,3))
plot(ethanol$NOx, acefit$ty, xlab = "NOx", ylab = expression(theta(NOx)))
plot(x[,1], avasfit$tx[,1], xlab = "C", ylab = "f(C)")
plot(x[,2], avasfit$tx[,2], xlab = "E", ylab = "f(E)")



# ----------
i <- order(ozone$O3)

par(mfrow=c(1,1))
plot(ozone$O3[i], avasfit$ty[i], type = "l", xlab = "O3", ylab = expression(theta(O3)))
gs <- lm(avasfit$ty[i] ~ sqrt(ozone$O3[i]))
lines(ozone$O3[i], gs$fit, lty = 2)
gl <- lm(avasfit$ty[i] ~ log(ozone$O3[i]))
lines(ozone$O3[i], gl$fit, lty = 5)


# -->
# original O3 fit as solid line,
# square-root fit as a dotted line and log fit as a dashed line.
# neither one fits well across the whole range.



# ----------
lmod <- lm(avasfit$ty ~ avasfit$tx)

summary(lmod)

plot(predict(lmod), residuals(lmod), xlab = "Fitted", ylab = "Residuals")


# -->
# the fit is not so good, but check the diagnostics.
# AVAS does not optimize the fit. It trades some of the optimality in order to obtain constant variance.
# Whether this is a good trade depends on how much relative value you put on the accuracy of point predictions and accurate estimation of the standard error of prediction.



