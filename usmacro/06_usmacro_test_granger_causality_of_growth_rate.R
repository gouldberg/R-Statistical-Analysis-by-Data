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


# ----------
u <- ts(usmacro$u, start = c(1948, 1), end = c(2016, 1), frequency = 4)

g <- ts(usmacro$g, start = c(1948, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# Granger Causality
# - a variable Unemployment Rate does not "Granger" cause another variable "Growth Rate", if the lags of "Unemployment Rate" are not significant in the ARDL model
# - Testing for Granger causality is equivalent to test the hypothesis that all the lags of an independent variable are insignificant
# ------------------------------------------------------------------------------
# ARDL(2,1) model
u21 <- dynlm(u ~ L(u, 1:2) + L(g, 1))



# ----------
hnull <- "L(g, 1) = 0"

lH <- linearHypothesis(u21, hnull)

kable(tidy(lH), digits = 4, align = "c", caption = "Granger Causality Test for the Unemployment Equation")


# -->
# With a p-value of 0.0139, the test rejects the null hypothesis at the 5% level, implying that g Granger causes u.



# ----------
# ARDL(2,4) model
u24 <- dynlm(u ~ L(u, 1:2) + L(g, 1:4))

h0 <- c("L(g, 1:4)1 = 0", "L(g, 1:4)2 = 0", "L(g, 1:4)3 = 0", "L(g, 1:4)4 = 0")

lH <- linearHypothesis(u24, h0)

kable(tidy(lH), digits = 4, align = "c", caption = "Granger Causality Test with Four Lags of the Growth Rate")


# -->
# Again, The test rejects the null hypothesis, implying that g Granger causes u.
# H0: all four lags are zero
# H1: at least one lag <> 0
