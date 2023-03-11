setwd("//media//kswada//MyFiles//R//parzen")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: parzen
#   - Variables:
#        - snowfall:  the annual snowfall in Buffalo, NY (inches) from 1910 to 1972 inclusive
# ------------------------------------------------------------------------------

data("parzen", package = "gamlss.data")


str(parzen)

car::some(parzen)



# ------------------------------------------------------------------------------
# Select the distribution
# ------------------------------------------------------------------------------

# We are using the default value for the argument type = "realAll", meaning we are using
# all available continuous distribution


# AIC and SBC

mod1 <- fitDist(snowfall, data=parzen, k=2, type = "realAll")

mod2 <- fitDist(snowfall, data=parzen, k=log(dim(parzen)[1]))



# ----------
# Weibull distribution is the best, altough several other distributions (including the normal)
# have similar values of AIC and SBC

mod1$fit

mod2$fit


# WEI3(mu, sigma) distribution is the parameterization of the Weibull distribution with mu (the mean)



# ------------------------------------------------------------------------------
# Refit and plot
# ------------------------------------------------------------------------------

# mu and sigma is log link
show.link("WEI3")


# Weibull distribution and a kernel density estimate (dashed)
m1 <- histDist(parzen$snowfall, "WEI3", density = TRUE, line.col = c(1,1), line.ty = c(1,2))



# ----------
# check the model
plot(m1)

wp(m1)



# -->
# Adequacy of the WEI3 distribution is indicated by over 95% of the deviations in the worm plot
# lying within the dashed (approximate 95%) confidence bands.





