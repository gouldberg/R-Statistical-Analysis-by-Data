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
# Spectral analysis:  Periodogram
# ------------------------------------------------------------------------------

n <- length(star)


# raw periodogram
( Per <- Mod(fft(star - mean(star))) ^ 2 / n )


( Freq <- (1:n-1) / n )



# ----------
graphics.off()
par(mfrow = c(2,1), mar = c(3,3,1,1), mgp = c(1.6, 0.6, 0))

plot(star, ylab = "star magnitude", xlab = "day")
plot(Freq[1:50], Per[1:50], type = "h", lwd = 3, ylab = "Periodogram", xlab = "Frequency")



# -->
# Periodogram ordinates for frequencies higher than 0.08 are essentially zero.



# ----------
( u <- which.max(Per[1:50]) )
( uu <- which.max(Per[1:50][-u]) )


1 / Freq[u]
1 / Freq[uu]


text(0.05, 7000, "24 day cycle")
text(0.027, 9000, "29 day cycle")


# -->
# Note that the 29 (= 1/0.035) day cycle and the 24 (= 1 / 0.041) day cycle are the most prominent periodic components of the data.


# -->
# We can interpret this result as we are observing an amplitude modulated signal.
# For example, suppose we are observing signal-plus-noise, x(t) = s(t) + v(t), where s(t) = cos(2*pi*omega*t) * cos(2*pi*delta*t),
# and delta is very small. In this case, the process will oscillate at frequency omega, but the amplitude wil be modulated by cos(2*pi*delta*t).
# Since 2 * cos(alpha) * cos(delta) = cos(alpha + delta) + cos(alpha - delta), the periodogram of data generated as x(t)
# will have two peaks close to each other at alpha +- delta.



# ----------
# another way to fint the two peaks is to order on Per
y <- cbind(1:50, Freq[1:50], Per[1:50])

y[order(y[,3]),]




# ------------------------------------------------------------------------------
# Amplitude modulated signal
# ------------------------------------------------------------------------------

t <- 1:200

alpha <- 0.2
delta <- 0.01



# ----------
x <- 2 * cos(2 * pi * alpha * t) * cos(2 * pi * delta * t)

x2 <- cos(2 * pi * (alpha - delta) * t) + cos(2 * pi * (alpha + delta) * t)



# ----------
graphics.off()
par(mfrow = c(1,1), mar = c(3,3,1,1), mgp = c(1.6, 0.6, 0))

plot.ts(x)

# the same
lines(x2, col = 2)



# -----------
Px <- Mod(fft(x))^2

plot(0:199/200, Px, type = "o")

