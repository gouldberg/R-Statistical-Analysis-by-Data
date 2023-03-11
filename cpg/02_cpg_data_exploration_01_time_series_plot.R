setwd("//media//kswada//MyFiles//R//cpg")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cpg
# ------------------------------------------------------------------------------

data(cpg, package = "astsa")

str(cpg)

cpg



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

par(mfrow = c(3, 1))

plot(cpg, type = "l", main = "CPG")

plot(log(cpg), type = "l", main = "Logged CPG")

plot(diff(log(cpg)), type = "l", main = "Dif of Logged CPG")
abline(h = mean(diff(log(cpg))), lty = 2, col = "blue")



# ----------
forecast::ndiffs(cpg)



# ----------
mean(diff(log(cpg)))


# -->
# -0.53% decreasing annually
