setwd("//media//kswada//MyFiles//R//differential_equations")

packages <- c("dplyr", "deSolve")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Population growth by logistic equation
#   - model the density changes of one species (y) in an environment where competition for available resources reduces
#     population growth at high densities, and eventually leads to negative growth above a specific carrying capacity K.
#     At very low density, the absence of competition allows exponential growth, at a growth rate r > 0.
#     y' = r * y (1 - y / K)
# ------------------------------------------------------------------------------


r <- 1

K <- 10



# ----------
yini <- 2


derivs <- function(t, y, parms) list(r * y * (1 - y / K))

times <- seq(from = 0, to = 20, by = 0.2)

out <- ode(y = yini, times = times, func = derivs, parms = NULL)

head(out)



# ----------
yini <- 12

out2 <- ode(y = yini, times = times, func = derivs, parms = NULL)


plot(out, out2, main = "logistic growth", lwd = 2)

