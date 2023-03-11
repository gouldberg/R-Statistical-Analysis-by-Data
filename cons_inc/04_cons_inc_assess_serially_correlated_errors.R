# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/cons_inc")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cons_inc
#    - y:  permanent income, an infinite stream of income
#    - cons:  consumption
# ------------------------------------------------------------------------------
data("cons_inc", package = "POE5Rdata")

data <- cons_inc

glimpse(data)

str(data)



# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,4)], start = c(1959, 3), end = c(2016, 3), frequency = 4)



# ----------
# Consumption Function
modC <- dynlm(d(cons, 1) ~ L(d(cons, 1)) + d(y, 1), data = data.ts)



# ------------------------------------------------------------------------------
# Model assessment for serially correlated errors
# ------------------------------------------------------------------------------

graphics.off()
plot(residuals(modC))
lines(lowess(residuals(modC)), col="blue")


# -->
# We can see some positive trend and larger fluctuation



# ------------------------------------------------------------------------------
# Model assessment:  Check residual correlogram for serially correlated errors
# ------------------------------------------------------------------------------

# Residual Correlogram of phillips curve model
acf(residuals(modC), main = "")


# -->
# but not serial correlation in the erros



# ------------------------------------------------------------------------------
# Model assessment:  Serially Correlated Errors:  Breusch-Godfrey Autocorrelation Test for higher-order serial correlation by lmtest::bgtest()
#   - can test for autocorrelation of higher orders, which requires including higher lags of errors
# ------------------------------------------------------------------------------

a1 <- lmtest::bgtest(modC, order = 1, type = "Chisq")
b1 <- lmtest::bgtest(modC, order = 2, type = "Chisq")
c1 <- lmtest::bgtest(modC, order = 3, type = "Chisq")
d1 <- lmtest::bgtest(modC, order = 4, type = "Chisq")
e1 <- lmtest::bgtest(modC, order = 5, type = "Chisq")
f1 <- lmtest::bgtest(modC, order = 6, type = "Chisq")
g1 <- lmtest::bgtest(modC, order = 7, type = "Chisq")

dfr <- data.frame(rbind(
  c(a1$statistic, a1$p.value),
  c(b1$statistic, b1$p.value),
  c(c1$statistic, c1$p.value),
  c(d1$statistic, d1$p.value),
  c(d1$statistic, e1$p.value),
  c(d1$statistic, f1$p.value),
  c(d1$statistic, g1$p.value)
))


dfrm <- cbind(1:7, dfr)

names(dfrm) <- c("k", "Chisq", "p-Value")

kable(dfrm, digits = 4, align="c", caption = "Breusch-Godfrey Test for Phillips Curve Model")


# -->
# indicating NO serial correlation



# ------------------------------------------------------------------------------
# Model assessment:  Serially Correlated Errors:  Durbin-Watron Test
#   - less and less used due to its limitaions, but may be considered when the sample is small
# ------------------------------------------------------------------------------
lmtest::dwtest(modC)


# -->
# H0 is not rejected, indicating NO serial correlation


