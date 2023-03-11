setwd("//media//kswada//MyFiles//R//oil_gas")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oil and Gas
# ------------------------------------------------------------------------------

data(oil, package = "astsa")
data(gas, package = "astsa")

str(oil)
str(gas)

oil
gas



# ------------------------------------------------------------------------------
# Regression with lagged variables and growth indicator
#   - There have been a number of studies questioning whether gasoline prices respond more quickly when oil prices are rising than
#     when oil prices are falling ("asymmetry").
#     We will attemp to explore this question here with simple lagged regression;
#     We will ignore some obvious problems such as outliers and autocorrelated errors, so this will not be a definitive analysis.
# ------------------------------------------------------------------------------

poil <- diff(log(oil))


pgas <- diff(log(gas))


# Indicator of no growth or positive growth in oil price
indi <- ifelse(poil < 0, 0, 1)



# ----------
mess <- ts.intersect(pgas, poil, poilL = stats::lag(poil, -1), indi)


head(mess)


fit <- lm(pgas ~ poil + poilL + indi, data = mess)


summary(fit)



# -->
# indi is highly significant.



# ----------
par(mfrow = c(2,2))

plot(fit)


plot(fitted(fit) ~ time(gas))




# ----------
acf2(resid(fit))

