# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/phillips5_aus")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  phillips5_aus
# ------------------------------------------------------------------------------
data("phillips5_aus", package = "POE5Rdata")

data <- phillips5_aus

glimpse(data)

str(data)

dim(data)


# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,3,4)], start = c(1987, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# OLS estimate of the Phillips curve equation
#   - inf(t) = alpha + beta0 + D(u(t)) + e(t)
# ------------------------------------------------------------------------------
phill.dyn <- dynlm(inf ~ diff(u), data = data.ts)


kable(tidy(phill.dyn), digits=3, caption = "Summary of the Phillips Model")



# ------------------------------------------------------------------------------
# Model assessment for serially correlated errors
# ------------------------------------------------------------------------------

graphics.off()
plot(residuals(phill.dyn))
lines(lowess(residuals(phill.dyn)), col="blue")


# -->
# We can see some negative trend before 1997



# ------------------------------------------------------------------------------
# Model assessment:  Check residual correlogram for serially correlated errors
# ------------------------------------------------------------------------------

# Residual Correlogram of phillips curve model
acf(residuals(phill.dyn), main = "")


# -->
# Shows serial correlation in the errors at lag 1 - 6



# ------------------------------------------------------------------------------
# Model assessment:  Serially Correlated Errors:  Breusch-Godfrey Autocorrelation Test for higher-order serial correlation by lmtest::bgtest()
#   - can test for autocorrelation of higher orders, which requires including higher lags of errors
# ------------------------------------------------------------------------------

a1 <- lmtest::bgtest(phill.dyn, order = 1, type = "Chisq")
b1 <- lmtest::bgtest(phill.dyn, order = 2, type = "Chisq")
c1 <- lmtest::bgtest(phill.dyn, order = 3, type = "Chisq")
d1 <- lmtest::bgtest(phill.dyn, order = 4, type = "Chisq")
e1 <- lmtest::bgtest(phill.dyn, order = 5, type = "Chisq")
f1 <- lmtest::bgtest(phill.dyn, order = 6, type = "Chisq")
g1 <- lmtest::bgtest(phill.dyn, order = 7, type = "Chisq")

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



# ------------------------------------------------------------------------------
# Model assessment:  Serially Correlated Errors:  Durbin-Watron Test
#   - less and less used due to its limitaions, but may be considered when the sample is small
# ------------------------------------------------------------------------------
lmtest::dwtest(phill.dyn)




