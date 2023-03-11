setwd("//media//kswada//MyFiles//R//differential_equations")

packages <- c("dplyr", "deSolve")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# The Lorenz equations
#   - The first chaotic dynamical system of ordinary differential equations to be described.
#     They consist of three ordinary differential equations, expressing the dynamics of the variables, X, Y, and Z that were assumed to represent
#     idealized behavior of the Earth's atmosphere.
#
#      - X' = a * X + Y * Z
#      - Y' = b * (Y - Z)
#      - Z' = - X * Y + c * Y - Z
#        X, Y, and Z refer to the horizontal and vertical temperature distribution and convective flow
# ------------------------------------------------------------------------------


a <- - 8 / 3

b <- -10

c <- 28


yini <- c(X = 1, Y = 1, Z = 1)


Lorenz <- function(t, y, parms){
  with(as.list(y), {
    dX <- a * X + Y * Z
    dY <- b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    list(c(dX, dY, dZ))
  })
}


times <- seq(from = 0, to = 100, by = 0.01)

out <- ode(y = yini, times = times, func = Lorenz, parms = NULL)

head(out)


# ----------
# deSolve's method plot depicts all variables at the same time, neatly arranged in several rows and columns
plot(out, lwd = 2)


plot(out[, "X"], out[,"Y"], type = "l", xlab = "X", ylab = "Y", main = "butterfly")
