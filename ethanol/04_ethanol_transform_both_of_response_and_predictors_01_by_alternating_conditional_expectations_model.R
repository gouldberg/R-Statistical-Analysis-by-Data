setwd("//media//kswada//MyFiles//R//ethanol")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ethanol
# ------------------------------------------------------------------------------

data(ethanol, package="lattice")

str(ethanol)



# ------------------------------------------------------------------------------
# Alternating Conditional Expectation
#  - The transform both sides (TBS) model:  theta(y) = alpha + sum(fj(Xj)) + e
#    One particular way of fitting TBS model is alternating conditional expectation which is designed to minimize sum(theta(yi) - sum(fj(xij))^2)
#    To avoid fj = 0, we impose the restriction that the variance of theta(y) to be one.
#
#  - ACE can be useful in searching for good transformations while building a linear model.
#    But caution is necessary if the model is to be used in its own right, because of the tendency to overfit.
# ------------------------------------------------------------------------------

library(acepack)



# ----------
# We start with the same 3 predictors.
# We must give it the X matrix explicitly.
# The function returns the components ty which contains theta(y) and tx which is a matrix whose columns cona\tain the fj(xj)

x <- ethanol[c("C", "E")]

acefit <- ace(x, ethanol$NOx)

acefit$ty

acefit$tx



# ----------
# We can get a sense of how well these transformations work by fitting a linear model that uses the transformed variables
# The intercept is zero so we exclude it to get cleaner output. 

summary(lm(ty ~ tx - 1, acefit))


# -->
# All 2 transformed predictors are strongly sifnificant and the fit is superior (=0.9629) to the original model



# ----------
# Now we examine the transforms on the response and the 3 predictors

par(mfrow=c(1,3))
plot(ethanol$NOx, acefit$ty, xlab = "NOx", ylab = expression(theta(NOx)))
plot(x[,1], acefit$tx[,1], xlab = "C", ylab = "f(C)")
plot(x[,2], acefit$tx[,2], xlab = "E", ylab = "f(E)")


