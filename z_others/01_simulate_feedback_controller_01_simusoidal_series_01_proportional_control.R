
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Generate Reference Time Series
# ------------------------------------------------------------------------------

# time length
n <- 1000


# cycles per unit time (here: t)
cyc <- c(n * 0.03, n * 0.1, n * 0.4)


# a:  cosine coefficients for 3 time series
a <- c(2, 4, 6)



# b:  sine coefficients for 3 time series
b <- c(3, 5, 7)




# ----------
# generate time series
# different frequency and amplitude

x1 <- a[1] * cos(2 * pi * 1:n * cyc[1] / n) + b[1] * sin(2 * pi * 1:n * cyc[1] / n)


x2 <- a[2] * cos(2 * pi * 1:n * cyc[2] / n) + b[2] * sin(2 * pi * 1:n * cyc[2] / n)


x3 <- a[3] * cos(2 * pi * 1:n * cyc[3] / n) + b[3] * sin(2 * pi * 1:n * cyc[3] / n)




# ----------
# mixtures of periodic series with multiple frequencies and amplitudes

xall <- x1 + x2 + x3





# ------------------------------------------------------------------------------
# Simulation of Feedback Contoller:  Proportional control
# ------------------------------------------------------------------------------


# Select Reference value
R <- x1



# ----------
# proportional control

Kp <- 0.8

C <- rep(Kp, n)



# ----------
# value for transfer function:  P  = b / (s + a)
# --> impulse response function y(t) = K / T * exp(-1 / T * t)

a <- 0.001

b <- 1

T <- 1 / a

K <- b / a

t <- 1:n
irf <- K / T * exp(-1 / T * t)



# ----------
# disturbance
D <- rnorm(n = n, mean = 0, sd = 0.5)




# ----------
# simulate Y (object to be controlled)

Y <- E <- U <- rep(0, n)

Y[1] <- 1



for(t in 2:(n-1)){

  # E:  error
  E[t] <- R[t] - Y[t-1]
  
  
  # U:  operational input to Y  (proportional control)
  U[t] <- C[t] * E[t]
  
  
  # D:  disturbance
  # P[s] = b / (s + a) --> K/T * exp(-1/T*t)
  for(t2 in t:(n-1)){
    Y[t2] <- Y[t2] + K / T * exp(- 1 / T * t2) * (U[t] + D[t])
  }
}




# ----------
# plot output

graphics.off()

par(mfrow = c(2,2))


plot(R, type = "l", lwd = 2, col = "black", main = "R(black): Reference   Y(blue): object")
lines(Y, lwd = 2, col = "blue")


plot(U + D, type = "l", lwd = 2, col = "red", main = "Red  =  U(black): Input to Controller + D(gray): disturbances")
lines(U , lwd = 1, col = "black")
lines(D, lwd = 1, col = "gray")


plot(irf, type = "l", lwd = 2, col = "black", main = paste0("P: IRF     K = ", K, "  T = ", T))

plot(E, type = "l", lwd = 1, col = "black", main = "E: error")
abline(h = 0, lty = 2, col = "gray")



