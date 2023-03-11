setwd("//media//kswada//MyFiles//R//differential_equations")

packages <- c("dplyr", "deSolve")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Rigid Body Equations:  Euler equations of a rigid body without external forces
#   - y'1 = (I2 - I3) / I1 * y2 * y3
#   - y'2 = (I3 - I1) / I2 * y1 * y3
#   - y'3 = (I1 - I2) / I3 * y1 * y2
#
#   - y1, y2, y3:  coordinates of the rotation vector
#   - I1, I2, I3:  principal moments of inertia
# ------------------------------------------------------------------------------


rigidode <- function(t, y, parms){
  dy1 <- -2 * y[2] * y[3]

  dy2 <- 1.25 * y[1] * y[3]
  
  dy3 <- -0.5 * y[1] * y[2]
  
  list(c(dy1, dy2, dy3))
}



yini <- c(1, 0, 0.9)

times <- seq(from = 0, to = 20, by = 0.01)



# ----------
# Solved with Cash-Karp 5(4) Runge-Kutta formula.
rkMethod("rk45ck")

out <- ode(times = times, y = yini, func = rigidode, parms = NULL, method = rkMethod("rk45ck"))

head(out)



# ----------
par(mfrow=c(1,1))
matplot(x = out[,1], y = out[,-1], type = "l", lwd = 2, lty = "solid", col = c("red", "blue", "black"), xlab = "time", ylab = "y", main = "rigidode")
legend("bottomright", col = c("red", "blue", "black"), legend = c("y1", "y2", "y3"), lwd = 2)


library(scatterplot3d)
scatterplot3d(out[,-1], type = "l", lwd = 2, xlab = "", ylab = "", zlab = "", main = "rigidode")
