setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# Model sigma as a function of explanatory variables
# ------------------------------------------------------------------------------

r4 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc, sigma.fo = ~pb(Fl) + pb(A) + H + loc, family = GA, data = rent)


r5 <- gamlss(R ~ pb(Fl) + pb(A) + H + loc, sigma.fo = ~pb(Fl) + pb(A) + H + loc, family = IG, data = rent)


AIC(r3, r4, r5)



# -->
# The smallest value of AIC for r4 indicates that the gamma distribution fits better than the inverse Gaussian (model r5).



# ------------------------------------------------------------------------------
# plot the fitted terms for sigma for model r4
# ------------------------------------------------------------------------------

par(mfrow=c(1,1))
term.plot(r4, pages = 1, what = "sigma", ask = FALSE)



# ------------------------------------------------------------------------------
# Test approximate significance of the terms for sigma by drop1()
# ------------------------------------------------------------------------------

drop1(r4, what = "sigma")


# -->
# Every term apart from H seems to contribute significantly to explaining the behaviour of the sigma parameter



# ------------------------------------------------------------------------------
# Check the adequacy of the distribution
# ------------------------------------------------------------------------------

plot(r4)

wp(r4, ylim.all = 0.6)


# -->
# There are a few points of the worm plot falling outside the 95% pointwise confidence intervals,
# indicating that the distribution may be inadequate ...

# Furthermore, the inverted U-shape of the residuals indicates negative skewness in the residuals and suggests that the gamma distribution might not be
# flexible enough to capture the skewness in the data.

# Thus, we will fit a more general GAMLSS model.


