# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/usmacro")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  usmacro
#
# u:  U.S. unemployment rate over a period of more than six decades
# g:  U.S. growth rate
# ------------------------------------------------------------------------------
data("usmacro", package = "POE5Rdata")

data <- usmacro

glimpse(data)

str(data)


is.ts(usmacro)

u.ts <- ts(usmacro$u, start = c(1948, 1), end = c(2016, 1), frequency = 4)

g.ts <- ts(usmacro$g, start = c(1948, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# Check time series plot
# ------------------------------------------------------------------------------
par(cex = 1.2, lwd = 1.8, mfrow=c(3,1))

ts.plot(u.ts, ylab = "Unemployment Rate", xlab = NULL)

ts.plot(diff(u.ts), ylab = "Difference in Unemployment Rate", xlab = NULL)

ts.plot(g.ts, ylab = "Growth Rate", xlab = NULL)


# -->
# The fluctuations in U.S. unemployment rate over a period of more than six decades.
# One my note that the unemployment rate in the financial crisis period (2008 - 2009) has reached the maximum of the previous three decades.

