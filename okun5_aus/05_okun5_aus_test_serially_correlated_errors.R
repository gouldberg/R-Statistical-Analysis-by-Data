# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/okun5_aus")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  okun5_aus
# ------------------------------------------------------------------------------
data("okun5_aus", package = "POE5Rdata")

glimpse(okun5_aus)

str(okun5_aus)


# remove dateid01
data <- okun5_aus[, c(2,3)]



# ----------
# convert to ts object
is.ts(data)

data.ts <- ts(data, start = c(1978, 2), end = c(2016, 2), frequency = 4)



# ----------
# FDL(4) model:  baed on Okun's Law
okunL4.dyn <- dynlm(d(u) ~ L(g, 0:4), data = data.ts)



# ------------------------------------------------------------------------------
# Model assessment:  residuals of FDL(4)
# ------------------------------------------------------------------------------

graphics.off()
plot(residuals(okunL4.dyn))
lines(lowess(residuals(okunL4.dyn)), col="blue")




# ------------------------------------------------------------------------------
# Model assessment:  Check residual correlogram for serially correlated errors
# ------------------------------------------------------------------------------

# Residual Correlogram of FDL(4)
acf(residuals(okunL4.dyn), main = "")


# -->
# Shows slight serial correlation in the erros at lag 1-2



# ------------------------------------------------------------------------------
# Model assessment:  Serially Correlated Errors:  Breusch-Godfrey Autocorrelation Test for higher-order serial correlation by lmtest::bgtest()
#   - can test for autocorrelation of higher orders, which requires including higher lags of errors
# ------------------------------------------------------------------------------

a1 <- lmtest::bgtest(okunL4.dyn, order = 1, type = "Chisq")
b1 <- lmtest::bgtest(okunL4.dyn, order = 2, type = "Chisq")
c1 <- lmtest::bgtest(okunL4.dyn, order = 3, type = "Chisq")
d1 <- lmtest::bgtest(okunL4.dyn, order = 4, type = "Chisq")

dfr <- data.frame(rbind(
  c(a1$statistic, a1$p.value),
  c(b1$statistic, b1$p.value),
  c(c1$statistic, c1$p.value),
  c(d1$statistic, d1$p.value)
))


dfrm <- cbind(1:4, dfr)

names(dfrm) <- c("k", "FDL(4) Chisq", "FDL(4) p-Value")

kable(dfrm, digits = 4, align="c", caption = "Breusch-Godfrey Test for Okun's Law FDL(4)")



# -->
# some serial correlation at lag 1



# ------------------------------------------------------------------------------
# Model assessment:  Serially Correlated Errors:  Durbin-Watron Test
#   - less and less used due to its limitaions, but may be considered when the sample is small
# ------------------------------------------------------------------------------
lmtest::dwtest(okunL4.dyn)


# -->
# Null Hypothesis: the autocorrelation of the disturbances is 0  --> rejected, indicating some serial correlation exists.
W
