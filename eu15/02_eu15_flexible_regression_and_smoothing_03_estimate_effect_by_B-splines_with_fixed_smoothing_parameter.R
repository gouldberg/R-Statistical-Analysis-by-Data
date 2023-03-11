setwd("//media//kswada//MyFiles//R//eu15")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  EU15
# ------------------------------------------------------------------------------
data("eu15", package = "gamlss.data")


str(eu15)

car::some(eu15)



# ----------
eu15 <- transform(eu15, lGDP = log(GDP), lCapital = log(Capital), lLabor = log(Labor), lUsefulEnergy = log(UsefulEnergy))

car::some(eu15)



# ------------------------------------------------------------------------------
# Estimate the effect of each variable
# ------------------------------------------------------------------------------
# pb(): penalized B-splines.
# Uses the B-spline basis of a piecewise polynomial of degree d with equally spaced knots over the x range.
# The default intervals = 20
# The default degree of the piecewise polynomial = 3


# Fix the smoothing parametera at 2.5
pbc <- pb.control(method = "GAIC", k = 2.5)

mod1 <- gamlss(lGDP ~ pb(lCapital, control = pbc) + pb(lUsefulEnergy, control = pbc) + pb(lLabor, control = pbc), data = eu15, family = TF2)



# ----------
summary(mod1)


plot(mod1)


wp(mod1)



# ----------
# plot the model terms
term.plot(mod1, pages = 1, ask = FALSE)



# ----------
# plot the fitted mean of lGDP with the data
summary(eu15$lCapital)
summary(eu15$lUsefulEnergy)
summary(eu15$lLabor)

neweu15 <- data.frame(expand.grid(lCapital = seq(8.5, 10.2, 0.1), lUsefulEnergy = seq(15.0, 16.2, 0.1), lLabor = seq(19.2, 19.5, 0.1)))


# ----------
neweu15$pred <- predict(mod1, newdata = neweu15, type = "response")


plot(pred ~ lCapital, data = neweu15, pch = ".")

plot(pred ~ lUsefulEnergy, data = neweu15, pch = ".")

plot(pred ~ lLabor, data = neweu15, pch = ".")



# ------------------------------------------------------------------------------
# Calculate the partial effect and the elasticity of a continuous explanatory variable
# ------------------------------------------------------------------------------

# getPEF(): get the partial effect function from a continuous term in a GAMLSS model
der.capital <- getPEF(mod1, data =~ eu15, term = "lCapital", plot = T)

der.labor <- getPEF(mod1, data =~ eu15, term = "lLabor", plot = T)

der.energy <- getPEF(mod1, data =~ eu15, term = "lUsefulEnergy", plot = T)




# ----------
# Estimate the average derivative, which gives us the effect on (log) GDP
der.capital(mean(eu15$lCapital), deriv = 1)

der.labor(mean(eu15$lLabor), deriv = 1)

der.energy(mean(eu15$lUsefulEnergy), deriv = 1)


summary(mod1)

