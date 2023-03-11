setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ----------
lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)

summary(lmod)



# ----------
mod_obj <- lmod



# ------------------------------------------------------------------------------
# Standardized Pearson residuals by index
# ------------------------------------------------------------------------------

# standardized Pearson residuals
stand.resid <- rstandard(mod_obj, type = "pearson")



# ----------
par(mfrow=c(1,1))

plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Pearson residuals", type = "l")

abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))


