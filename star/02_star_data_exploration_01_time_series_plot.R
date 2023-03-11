setwd("//media//kswada//MyFiles//R//star")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  star
# ------------------------------------------------------------------------------

data(star, package = "astsa")


str(star)

head(star)



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1), mar = c(3,3,1,1), mgp = c(1.6, 0.6, 0))

plot(star, ylab = "star magnitude", xlab = "day")



# ----------
forecast::ndiffs(star)