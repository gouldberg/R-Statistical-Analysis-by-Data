setwd("//media//kswada//MyFiles//R//grip")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  grip
# ------------------------------------------------------------------------------

data("grip", package = "gamlss.data")


str(grip)

car::some(grip)



# ------------------------------------------------------------------------------
# Extract fitted parameters of LMS model using BCCGo distribution as starting values
# ------------------------------------------------------------------------------

mu.start <- exp(unname(m0$mu.coefficients[1]))

sigma.start <- exp(unname(m0$sigma.coefficients[1]))

nu.start <- unname(m0$nu.coefficients[1])




# ------------------------------------------------------------------------------
# Fit other distribution with fitted values of LMS model using BCCGo (m0) as starting value
# ------------------------------------------------------------------------------

m1 <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), tau.fo = ~pb(age), family = BCTo, data = grip)


m1_s <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), tau.fo = ~pb(age), family = BCTo, data = grip, start.from = m0)


m2 <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), tau.fo = ~pb(age), family = BCPEo, data = grip)


m2_s <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), tau.fo = ~pb(age), family = BCPEo, data = grip, start.from = m0)




# ------------------------------------------------------------------------------
# Fit other distribution with fitted parameters values of LMS model using BCCGo (m0) as starting value
# ------------------------------------------------------------------------------

m1_s2 <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), tau.fo = ~pb(age), family = BCTo, data = grip, 
                mu.start = mu.start, sigma.start = sigma.start, nu.start = nu.start)


m2_s2 <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), tau.fo = ~pb(age), family = BCPEo, data = grip, 
                mu.start = mu.start, sigma.start = sigma.start, nu.start = nu.start)



# ----------
# compare models
GAIC(m0, m1, m1_s, m1_s2, m2, m2_s, m2_s2, k = 2)

GAIC(m0, m1, m1_s, m1_s2, m2, m2_s, m2_s2, k = log(nrow(grip)))



# ----------
edfAll(m0)

edfAll(m1)

edfAll(m2)



# ------------------------------------------------------------------------------
# Plot the fitted parameters
# ------------------------------------------------------------------------------

# LMS method using BCCGo distribution
fittedPlot(m0, x = grip$age)


# BCTo and BCPEo distribution model
fittedPlot(m1_s, x = grip$age)

fittedPlot(m2_s, x = grip$age)



