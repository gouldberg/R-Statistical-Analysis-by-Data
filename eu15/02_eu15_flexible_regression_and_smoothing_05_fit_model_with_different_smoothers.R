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
# Fitting different smoothers:  cubic splines
# ------------------------------------------------------------------------------

mod4 <- gamlss(lGDP ~ cs(lCapital) + cs(lUsefulEnergy) + cs(lLabor), data = eu15, family = TF2)
               
mod5 <- gamlss(lGDP ~ cs(lCapital) + cs(lUsefulEnergy) + cs(lLabor), 
               sigma.fo = ~ cs(lCapital) + cs(lUsefulEnergy) + cs(lLabor), 
               nu.fo = ~ cs(lCapital) + cs(lUsefulEnergy) + cs(lLabor), what = c("mu", "sigma", "nu"), data = eu15, family = TF2)

summary(mod4)

summary(mod5)


# ----------
plot(mod4)

plot(mod5)

wp(mod4)

wp(mod5)


# ----------
term.plot(mod1, what = "mu", page = 1, ask = FALSE)
term.plot(mod4, what = "mu", page = 1, ask = FALSE)
term.plot(mod5, what = "mu", page = 1, ask = FALSE)

term.plot(mod3, what = "sigma", page = 1, ask = FALSE)
term.plot(mod5, what = "sigma", page = 1, ask = FALSE)

term.plot(mod3, what = "nu", page = 1, ask = FALSE)
term.plot(mod5, what = "nu", page = 1, ask = FALSE)



# -->
# sigma's 95% interval is narrower for cubis spline model (mod5)
# partial effect for nu is very different between mod3 (B-spline) and mod5 (cubic splines)



# ----------
GAIC(mod1, mod3, mod4, mod5)



# -->
# cubic splines with no smoothing function for sigma and nu (mod4) is the best ...  




# ------------------------------------------------------------------------------
# Estiamte the derivatives for mu
# ------------------------------------------------------------------------------

# mu
der.capital <- getPEF(mod1, data =~ eu15, term = "lCapital", parameter = "mu", plot = T)
der.capital3 <- getPEF(mod3, data =~ eu15, term = "lCapital", parameter = "mu", plot = T)
der.capital4 <- getPEF(mod4, data =~ eu15, term = "lCapital", parameter = "mu", plot = T)
der.capital5 <- getPEF(mod5, data =~ eu15, term = "lCapital", parameter = "mu", plot = T)


der.labor <- getPEF(mod1, data =~ eu15, term = "lLabor", parameter = "mu", plot = T)
der.labor3 <- getPEF(mod3, data =~ eu15, term = "lLabor", parameter = "mu", plot = T)
der.labor4 <- getPEF(mod4, data =~ eu15, term = "lLabor", parameter = "mu", plot = T)
der.labor5 <- getPEF(mod5, data =~ eu15, term = "lLabor", parameter = "mu", plot = T)


der.energy <- getPEF(mod1, data =~ eu15, term = "lUsefulEnergy", parameter = "mu", plot = T)
der.energy3 <- getPEF(mod3, data =~ eu15, term = "lUsefulEnergy", parameter = "mu", plot = T)
der.energy4 <- getPEF(mod4, data =~ eu15, term = "lUsefulEnergy", parameter = "mu", plot = T)
der.energy5 <- getPEF(mod5, data =~ eu15, term = "lUsefulEnergy", parameter = "mu", plot = T)


