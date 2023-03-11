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
# Fit neural network smoothers interfacing with nnet():  the nn() function
# ------------------------------------------------------------------------------


# Scaling
# Note that A is year ... but it is scaled !!!
rent$Fls <- (rent$Fl - min(rent$Fl)) / (max(rent$Fl) - min(rent$Fl))

rent$As <- (rent$A - min(rent$A)) / (max(rent$A) - min(rent$A))



# ----------
library(gamlss.add)

set.seed(1432)


mr1 <- gamlss(R ~ nn(~ Fls + As, size = 5, decay = 0.01), data = rent, family = GA)


mr2 <- gamlss(R ~ nn(~ Fls + As, size = 5, decay = 0.01) + H + B + loc, data = rent, family = GA)


mr3 <- gamlss(R ~ nn(~ Fls + As + H + B + loc, size = 5, decay = 0.01), data = rent, family = GA)


AIC(mr1, mr2, mr3)

AIC(mr1, mr2, mr3, k = log(1969))


# -->
# Note that the results may depend on the chosen seed.



# ----------
# Fitted coefficients
# b:  constant intercept (bias)
# h:  hidden variable
# o:  output
summary(getSmo(mr3))



# ----------
# A visual presentation of the fitted neural network model
# Thicker lines represent coefficients with high values, while "black" and "grey" colours represent positive and negative values, respectively.
plot(getSmo(mr3), y.lab = expression(g[1](mu)))



# ------------------------------------------------------------------------------
# Fit a neural network model to the sigma parameter of the gamma distribution
# ------------------------------------------------------------------------------

mr4 <- gamlss(R ~ nn(~ Fls + As + H + B + loc, size = 5, decay = 0.1),
              sigma.fo = ~ nn(~ Fls + As + H + B + loc, size = 5, decay = 0.1), data = rent,
              family = GA, gd.tol = 1000)

AIC(mr2, mr3, mr4)

AIC(mr2, mr3, mr4, k = log(1969))


# -->
# Again the AIC selects the more complicated model mr4, while the SBC selects the simpler model mr2 !!



# ----------
plot(getSmo(mr4), y.lab = expression(g[1](m)))


plot(getSmo(mr4, what = "sigma"), y.lab = expression(g[2](sigma)))











