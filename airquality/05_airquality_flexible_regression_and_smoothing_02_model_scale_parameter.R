setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------
data("airquality")


str(airquality)

car::some(airquality)



# ------------------------------------------------------------------------------
# Model sigma as a function of explanatory variables
# ------------------------------------------------------------------------------

# gamlss() function does not work with NA's, so before fitting the model the cases with missing values have to be removed.
da <- na.omit(airquality)


# this produce errors...
# r4 <- gamlss(Ozone ~ pb(Temp) + pb(Wind) + pb(Solar.R), sigma.fo = ~pb(Temp) + pb(Wind) + pb(Solar.R), family = GA, data = da)


r5 <- gamlss(Ozone ~ pb(Temp) + pb(Wind) + pb(Solar.R), sigma.fo = ~pb(Temp) + pb(Wind) + pb(Solar.R), family = IG, data = da)


AIC(r3, r5)



# -->
# The smallest value of AIC for r5, significantly lower AIC




# ------------------------------------------------------------------------------
# plot the fitted terms for mu and sigma for model r5
# ------------------------------------------------------------------------------

par(mfrow=c(2,2))


term.plot(r5, pages = 1, what = "mu", ask = FALSE)



# -->
# Note that wind has different relationship .. 
# actually even if the wind is strong, the Ozone is large ...




# ----------
term.plot(r5, pages = 1, what = "sigma", ask = FALSE)



# -->
# Temperature has different sigma at low temp and high temp
# Note that May and September is basically low temperature, and its variation is large.




# ------------------------------------------------------------------------------
# Test approximate significance of the terms for sigma by drop1()
# ------------------------------------------------------------------------------


drop1(r5, what = "sigma")



# -->
# Every term seems to contribute significantly to explaining the behaviour of the sigma parameter




# ------------------------------------------------------------------------------
# Check the adequacy of the distribution
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))

wp(r5, ylim.all = 2)

wp(r3, ylim.all = 2)



# -->
# Better than model r3






