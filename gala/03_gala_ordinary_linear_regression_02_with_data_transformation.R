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
# Try square root data transformation
# ------------------------------------------------------------------------------


# transform Species to sqrt(Species)
modt <- lm(sqrt(Species) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)


summary(modt)



# -->
# Note Intercept is significant and Area is marginally ..



# ----------
par(mfrow=c(2,2))

plot(modt)



# ----------
par(mfrow=c(1,2))

plot(predict(modl), residuals(modl), xlab="Fitted", ylab="Redisuals")

lines(lowess(predict(modl), residuals(modl)), col = "blue", lty = 2)  

plot(predict(modt), residuals(modt), xlab="SQRT Fitted", ylab="SQRT Redisuals")

lines(lowess(predict(modt), residuals(modt)), col = "blue", lty = 2)

