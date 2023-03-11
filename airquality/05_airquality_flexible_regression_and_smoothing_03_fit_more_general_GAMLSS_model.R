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
# 3-parameter Box-Cox Cole and Green (BCCGo) distribution
# ------------------------------------------------------------------------------

# gamlss() function does not work with NA's, so before fitting the model the cases with missing values have to be removed.
da <- na.omit(airquality)


con <- gamlss.control(mu.step = 0.1, sigma.step = 0.1, nu.step = 0.1)

r6 <- gamlss(Ozone ~ pb(Temp) + pb(Wind) + pb(Solar.R), sigma.fo = ~pb(Temp) + pb(Wind) + pb(Solar.R), nu.fo = ~1, family = BCCGo, data = da, control = con)


r7 <- gamlss(Ozone ~ pb(Temp) + pb(Wind) + pb(Solar.R), sigma.fo = ~pb(Temp) + pb(Wind) + pb(Solar.R), nu.fo = ~pb(Temp) + pb(Wind) + pb(Solar.R), family = BCCGo, data = da, control = con)


AIC(r5, r6, r7)




# ----------
# Check the adequacy of the fitted distributions

par(mfrow = c(2,2))

wp(r5, ylim.all = 2);  title("r5: IV(mu, sigma)")

wp(r6, ylim.all = 2);  title("r6: BCCG(mu, sigma)")

wp(r7, ylim.all = 2);  title("r7: BCCG(mu, sigma, nu)")

