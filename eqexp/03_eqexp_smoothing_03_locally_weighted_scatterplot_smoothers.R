setwd("//media//kswada//MyFiles//R//eqexp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqexp
# ------------------------------------------------------------------------------

data(eqexp, package = "astsa")


head(eqexp)

str(eqexp)


attach(eqexp)


# ----------
P <- 1:1024

S <- P + 1024


x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S], EX6[S], NZ[S])

x.name <- c("EQ5-P", "EQ6-P", "EX5-P", "EX6-P", "NZ-P", "EQ5-S", "EQ6-S", "EX5-S", "EX6-S", "NZ-S")

colnames(x) <- x.name



# ------------------------------------------------------------------------------
# Smoothing in time series context
# Locally weighted scatterplot smoothers (lowess)
# ------------------------------------------------------------------------------
# Lowess:  nearest neighbor and robust weighted regression, wherein one uses only the data {x(t-k/2),...,x(t),...,x(t+k/2)}

obj_ts <- EQ5


par(mfrow=c(1,1))

plot(obj_ts, type = "l")


# Smoother uses 5% of the data
lines(lowess(obj_ts, f = 0.05), lwd = 2, col = 4)


# trend (with default smoother span f = 2 / 3)
lines(lowess(obj_ts), lty = 2, lwd = 2, col = 2)

