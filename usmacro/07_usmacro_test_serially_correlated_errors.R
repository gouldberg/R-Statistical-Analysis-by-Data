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


# ----------
# ARDL(1,1) and ARDL(2,1)
u11 <- dynlm(u.ts ~ L(u.ts, 1) + L(g.ts, 1))

u21 <- dynlm(u.ts ~ L(u.ts, 1:2) + L(g.ts, 1))



# ------------------------------------------------------------------------------
# Model assessment:  residuals of ARDL(2,1) and ARDL(1,1)
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))

plot(residuals(u11))
lines(lowess(residuals(u11)), col="blue")

plot(residuals(u21))
lines(lowess(residuals(u21)), col="blue")



# ------------------------------------------------------------------------------
# Model assessment:  Check residual correlogram for serially correlated errors
# ------------------------------------------------------------------------------

# Residual Correlogram of ARDL(2,1)
acf(residuals(u21), main = "")


# -->
# Shows no evidence of serial correlation in the errors for the ARDL(2,1)



# ------------------------------------------------------------------------------
# Model assessment:  Serially Correlated Errors:  Breusch-Godfrey Autocorrelation Test for higher-order serial correlation by lmtest::bgtest()
#   - can test for autocorrelation of higher orders, which requires including higher lags of errors
# ------------------------------------------------------------------------------
# The test for ARDL(1,1) and ARDL(2,1)
u11 <- dynlm(u.ts ~ L(u.ts, 1) + L(g.ts, 1))

u21 <- dynlm(u.ts ~ L(u.ts, 1:2) + L(g.ts, 1))



# ----------
a1 <- lmtest::bgtest(u11, order = 1, type = "Chisq")
b1 <- lmtest::bgtest(u11, order = 2, type = "Chisq")
c1 <- lmtest::bgtest(u11, order = 3, type = "Chisq")
d1 <- lmtest::bgtest(u11, order = 4, type = "Chisq")

a2 <- lmtest::bgtest(u21, order = 1, type = "Chisq")
b2 <- lmtest::bgtest(u21, order = 2, type = "Chisq")
c2 <- lmtest::bgtest(u21, order = 3, type = "Chisq")
d2 <- lmtest::bgtest(u21, order = 4, type = "Chisq")

dfr <- data.frame(rbind(
  c(a1$statistic, a1$p.value, a2$statistic, a2$p.value),
  c(b1$statistic, b1$p.value, b2$statistic, b2$p.value),
  c(c1$statistic, c1$p.value, c2$statistic, c2$p.value),
  c(d1$statistic, d1$p.value, d2$statistic, d2$p.value)
))


dfrm <- cbind(1:4, dfr)

names(dfrm) <- c("k", "(1,1) Chisq", "(1,1) p-Value", "(2,1) Chisq", "(2,1) p-Value")

kable(dfrm, digits = 4, align="c", caption = "Breusch-Godfrey Test for the Unemployment Equation")



# -->
# ARDL(1,1) model:  all the 4 tests reject the H0, indicating that serial correlation in the errors exists.
# ARDL(2,1) model:  1st and 4th order tests do not reject H0, but the 2nd and 3rd order tests do reject H0.

# -->
# This is different results by correlogram of residuals of ARDL(2,1) model checked above ....



# ------------------------------------------------------------------------------
# Model assessment:  Serially Correlated Errors:  Durbin-Watron Test
#   - less and less used due to its limitaions, but may be considered when the sample is small
# ------------------------------------------------------------------------------
lmtest::dwtest(u11)

lmtest::dwtest(u21)

