# setwd("//media//kswada//MyFiles//R//hawaii")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//hawaii")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hawaii
# ------------------------------------------------------------------------------

Hawaii <- read.table(file = "Hawaii.txt", header = TRUE)


str(Hawaii)


dim(Hawaii)


car::some(Hawaii)



# ------------------------------------------------------------------------------
# data exploration:  detrending and differencing
# ------------------------------------------------------------------------------


# regress sqrt(Moorhen.Kauai) on time
fit <- lm(sqrt(Moorhen.Kauai) ~ time(Moorhen.Kauai), na.action = na.exclude, data = Hawaii)



# ----------
par(mfrow = c(2, 1))

plot(resid(fit), type = "l", main = "detrended")

plot(diff(Hawaii$Moorhen.Kauai), type = "l", main = "first difference")



# -->
# One advantage of differencing over detrending to remove trend is that no parameters are estimated in the differencing operation.
# One disadvantage, however, is that differencing does not yield an estimate of the stationary process y(t).
# If an estimate of y(t) is essential, then detrending may be more appropriate.
# If the goal is to coerce the data to stationarity, then differencing may be more appropriate.

