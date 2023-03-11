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



# ------------------------------------------------------------------------------
# Finite Distributed Lag Model for 4 and 5 lags
#
# Okun's Law: u(t) - u(t-1) = - gamma * (g(t) - g(N))
# The econometric model:  D(u(t)) = alpha + beta0 * g(t) + beta1 * g(t-1) + ... + betaq * g(t-1) + e(T)
# ------------------------------------------------------------------------------

# Based on BIC(SC), optimal model of lags = 3, but here we try lags 4 and 5

okunL4.dyn <- dynlm(d(u) ~ L(g, 0:4), data = data.ts)
okunL5.dyn <- dynlm(d(u) ~ L(g, 0:5), data = data.ts)

kable(tidy(summary(okunL4.dyn)), digits = 4, caption = "The Okun Distributed Lag Model with 4 Lags")

kable(tidy(summary(okunL5.dyn)), digits = 4, caption = "The Okun Distributed Lag Model with 5 Lags")



# ----------
# We summarize Multiple R-squared, F-statistic, AIC, BIC
glL4 <- glance(okunL4.dyn)[c("r.squared", "statistic", "AIC", "BIC")]
glL5 <- glance(okunL5.dyn)[c("r.squared", "statistic", "AIC", "BIC")]
tabl <- rbind(as.numeric(glL4), as.numeric(glL5))
tabl <- cbind(Model = c("Four Lags", "Five Lags"), tabl)
tabl <- data.frame(tabl)
names(tabl) <- c("Model", "r.squared", "statistic", "AIC", "BIC")

kable(tabl, digits = 3, caption = "Statistics for Two Okun Models")


# -->
# FDL(4) is better than FLD(5)



# ------------------------------------------------------------------------------
# Calculate Interim Multiplers of FDL(4) model
#
#   - Delay Multipliers:  Coefficient of lags
#   - Impact Multipliers:  the order 0 coefficient, the immediate (contemporaneous) impact of a change in growth rate on difference of unemployment rate
#   - Interim Multipliers:  If x increase by one unit today, the change in y will be beta0 + beta1 + ... + betas after s periods
# ------------------------------------------------------------------------------
b <- coef(okunL4.dyn)[c(2:6)]
names(b) <- paste0("b", 0:4)
S <- numeric(5)
for(s in 1:5){S[[s]] <- sum(b[1:s])}
tab <- data.frame(b, S)
names(tab) <- c("Delay Multipliers", "Interim Multipliers")

kable(tab, digits = 4, align = "c", caption = "Multipliers for the Four-Lag Okun Model")


# -->
# If the growth rate changes in one unit at current quarter, 
# the change in the difference of unemployment rate will be -0.5276 after 4 quarters


