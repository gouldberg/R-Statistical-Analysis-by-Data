# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/housing_starts")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# --> continued from previous scripts

# ------------------------------------------------------------------------------
# Assess serially correlated residuals
# ------------------------------------------------------------------------------

# plot residuals
graphics.off()
plot(residuals(reg.koy))
lines(lowess(residuals(reg.koy)), col="blue")



# ----------
# Residual Correlogram
acf(residuals(reg.koy), main = "")


# -->
# Shows slight serial correlation in the erros at lag 1


# ----------
# Breusch-Godfrey Autocorrelation Test for higher-order serial correlation by lmtest::bgtest()
#   - can test for autocorrelation of higher orders, which requires including higher lags of errors

a1 <- lmtest::bgtest(reg.koy, order = 1, type = "Chisq")
b1 <- lmtest::bgtest(reg.koy, order = 2, type = "Chisq")
c1 <- lmtest::bgtest(reg.koy, order = 3, type = "Chisq")
d1 <- lmtest::bgtest(reg.koy, order = 4, type = "Chisq")

dfr <- data.frame(rbind(
  c(a1$statistic, a1$p.value),
  c(b1$statistic, b1$p.value),
  c(c1$statistic, c1$p.value),
  c(d1$statistic, d1$p.value)
))


dfrm <- cbind(1:4, dfr)

names(dfrm) <- c("k", "Chisq", "p-Value")

kable(dfrm, digits = 4, align="c", caption = "Breusch-Godfrey Test")


# -->
# H0 is rejected, indicating serially correlated residuals



# ----------
# Durbin-Watron Test
#   - less and less used due to its limitaions, but may be considered when the sample is small

lmtest::dwtest(reg.koy)


# ----------
car::durbinWatsonTest(reg.koy, max.lag = 4)


# ----------
acf1 = as.numeric(acf(resid(reg.koy), plot=F)$acf[2])
sumr = summary(reg.koy)
se = sumr$coef[2,2];  se2 = se^2;
bigt = length(resid(reg.koy))
( durbinh = acf1 * sqrt(bigt / (1 - bigt * se2)) )

if(abs(durbinh) > 1.96)  print("regr residuals are autocorrelated")


# -->
# Null Hypothesis: the autocorrelation of the disturbances is 0  --> rejected, indicating some serial correlation exists.


