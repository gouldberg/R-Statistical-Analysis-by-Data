setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")


str(econ5)


car::some(econ5)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

ts.plot(econ5, col = 1:5)


MTSplot(econ5)



# -->
# Note that all the series has some upward trend:  even for Unemp




# ----------
apply(econ5, MARGIN = 2, FUN = forecast::ndiffs)



# -->
# Note that consum has 2 order difference




# ----------
# 1st difference
MTSplot(mapply(FUN = diff, econ5))



# 2nd order difference
MTSplot(mapply(FUN = diff, MoreArgs = list(diff = 2), econ5))




# ------------------------------------------------------------------------------
# data exploration:  time series plot after data transformation
# ------------------------------------------------------------------------------

# Data transformation: First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))


dat <- cbind(G, U, C)


MTSplot(dat)




# ----------
apply(dat, MARGIN = 2, FUN = forecast::ndiffs)




# ------------------------------------------------------------------------------
# data exploration:  growth rate
# ------------------------------------------------------------------------------

gr <- diff(log(ts(econ5, start = 1948, frequency = 4)))

plot(100 * gr, main = "Growth Rates (%)")


