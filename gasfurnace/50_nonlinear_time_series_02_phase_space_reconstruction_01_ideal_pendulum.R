
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Ideal pendulum:  small oscillations
# ------------------------------------------------------------------------------

omega2 <- 3


parms <- c(1, omega2)

tinit <- 0

tfin <- 10


step <- 0.01

times <- seq(tinit, tfin, by = step)



# ----------
# system equations

# theta:  the angle with respect to the vertical


funct <- function(t, integ, p){
  
  # angle:  x(t) = theta(t)
  x <- integ[1]
  
  # angle velocity:  y(t) = d theta(t) / d t
  y <- integ[2]
  
  
  dx <- parms[1] * y
  
  dy <- - parms[2] * x
  
  list(c(dx, dy))
}



# ----------

library(deSolve)


cinit <- c(1,1)


# Runge-Kutta 4th order integration
xy <- rk4(cinit, times, funct, parms)


# also
# xy <- lsoda(cinit, times, func, parms)

head(xy)



# ----------
# phase space portrait
graphics.off()


par(mfrow = c(1,2))


plot(xy[,2], xy[,3], type = "l", xlim = c(-3, 3), ylim = c(-3, 3), xlab = "x(t)", ylab = "y(t)",
     cex.lab = 1.7, cex.axis = 1.3, lwd = 3, lty = 1)

plot(xy[,1], xy[,2], type = "l", xlab = "t", ylim = c(-2, 2), ylab = "x(t)",
     cex.lab = 1.7, cex.axis = 1.3, lwd = 4, lty = 1)


segments(0., 0, 10., 0, lty = 2, lwd = 3)

segments(0.9444, 0.50, 4.58, 0.50, lty = 4, lwd = 3, col = "black")

segments(7.57, 0, 7.57, 1.15, lty = 4, lwd = 3, col = "black")

text(2.76, 0.66, labels = "T", cex = 2, col = "black")

text(7.15, 0.25, labels = "A", cex = 2, col = "black")




# ------------------------------------------------------------------------------
# Embedding
# ------------------------------------------------------------------------------


# observed series
obss <- xy[,2]



# ----------
# m: dimension of the reconstructed phase space (embedding dimension)
# d: time delay

embedding <- function(x, m, d){
  
  n <- length(x)
  
  ne <- n - (m - 1) * d
  
  y <- matrix(0, ne, m)
  
  for(i in 1:m){
    y[,1] <- x[((i - 1) * d + 1) : (ne + (i - 1) * d)]
  }
  
  return(y)
}



# ----------

# if d is too small we have a bad reconstruction

y <- embedding(obss, m = 2, d = 50)


y



# ----------
graphics.off()

par(mfrow = c(1,2))

plot(xy[,2], xy[,3], type = "l", xlim = c(-3, 3), ylim = c(-3, 3), xlab = "x(t)", ylab = "y(t)",
     cex.lab = 1.7, cex.axis = 1.3, lwd = 3, lty = 1)

plot(y, type = "l", xlim = c(-3, 3), ylim = c(-3, 3), xlab = "x(t)", ylab = "y(t)", cex.lab = 1.7,
     cex.axis = 1.3, lwd = 3, lty = 1)



