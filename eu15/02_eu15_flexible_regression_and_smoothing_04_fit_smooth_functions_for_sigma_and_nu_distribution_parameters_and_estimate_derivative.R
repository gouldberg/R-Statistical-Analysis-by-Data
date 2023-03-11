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
# Fitting smooth functions for sigma and nu distribution parameters
# ------------------------------------------------------------------------------

show.link(TF2)


# ----------
mod2 <- update(mod1, sigma.fo = ~ pb(lCapital) + pb(lUsefulEnergy) + pb(lLabor), what = "sigma")

mod3 <- update(mod1, sigma.fo = ~ pb(lCapital) + pb(lUsefulEnergy) + pb(lLabor), 
               nu.fo = ~ pb(lCapital) + pb(lUsefulEnergy) + pb(lLabor), what = c("sigma", "nu"))

summary(mod3)



# ----------
plot(mod3)

wp(mod3)



# ----------
term.plot(mod1, what = "mu", page = 1, ask = FALSE)


term.plot(mod3, what = "mu", page = 1, ask = FALSE)

term.plot(mod3, what = "sigma", page = 1, ask = FALSE)

term.plot(mod3, what = "nu", page = 1, ask = FALSE)



# ------------------------------------------------------------------------------
# Estiamte the derivatives for mu, sigma and nu
# ------------------------------------------------------------------------------

# original mod1's mu and mod3's mu
der.capital <- getPEF(mod1, data =~ eu15, term = "lCapital", parameter = "mu", plot = T)
der.capital3 <- getPEF(mod3, data =~ eu15, term = "lCapital", parameter = "mu", plot = T)


der.labor <- getPEF(mod1, data =~ eu15, term = "lLabor", parameter = "mu", plot = T)
der.labor3 <- getPEF(mod3, data =~ eu15, term = "lLabor", parameter = "mu", plot = T)


der.energy <- getPEF(mod1, data =~ eu15, term = "lUsefulEnergy", parameter = "mu", plot = T)
der.energy3 <- getPEF(mod3, data =~ eu15, term = "lUsefulEnergy", parameter = "mu", plot = T)



# -->
# Partial effect function of lCapital to mu is different between mod1 and mod3



# ----------
# updated mod3's sigma
der.capital3_sigma <- getPEF(mod3, data =~ eu15, term = "lCapital", parameter = "sigma", plot = T)
der.labor3_sigma <- getPEF(mod3, data =~ eu15, term = "lLabor", parameter = "sigma", plot = T)
der.energy3_sigma <- getPEF(mod3, data =~ eu15, term = "lUsefulEnergy", parameter = "sigma", plot = T)



# ----------
# updated mod3's nu
der.capital3_nu <- getPEF(mod3, data =~ eu15, term = "lCapital", parameter = "nu", plot = T)
der.labor3_nu <- getPEF(mod3, data =~ eu15, term = "lLabor", parameter = "nu", plot = T)
der.energy3_nu <- getPEF(mod3, data =~ eu15, term = "lUsefulEnergy", parameter = "nu", plot = T)


