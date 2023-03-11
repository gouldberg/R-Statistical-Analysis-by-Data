setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ------------------------------------------------------------------------------
# Ordinary Linear Regression
# ------------------------------------------------------------------------------

# including variables except for Endemics
modl <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)


summary(modl)



# -->
# Only Elevation and Adjacent is significant
# Note that Adjacent itself is not correlated with Species



# ----------
par(mfrow = c(2,2))

plot(modl)



# ----------
par(mfrow = c(1,1))

plot(predict(modl), residuals(modl), xlab="Fitted", ylab="Redisuals")

lines(lowess(predict(modl), residuals(modl)), col = "blue", lty = 2)

