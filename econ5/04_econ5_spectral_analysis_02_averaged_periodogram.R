setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")

str(econ5)


car::some(econ5)




# ----------
# data transformation
# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))


x <- cbind(G, U, C)



# ----------
Ud <- diff(econ5$unemp)

Gd <- diff(econ5$govinv)

Cd <- diff(econ5$consum)



dat <- cbind(Gd, Ud, Cd)




# ------------------------------------------------------------------------------
# Spectral analysis:  averaged periodogram
#   - Previous analysis suggest the power in the lowe El Nino frequency needs smoothing to identify the predominant overall period.
# ------------------------------------------------------------------------------

# bandwidth = L / 160 = 0.05625

L <- 9

m <- (L - 1) / 2



# ----------
par(mfrow=c(2,2))


# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
Ud.ave <- astsa::mvspec(Ud, kernel("daniell", m), log = "no")

Gd.ave <- astsa::mvspec(Gd, kernel("daniell", m), log = "no")

Cd.ave <- astsa::mvspec(Cd, kernel("daniell", m), log = "no")




# ----------
Ud.ave$details


data.frame(Ud.ave$details) %>% arrange(desc(spectrum)) %>% head()



# -->
# largest spectrum is at frequency 0.0875, period = 11.42 quarters




# ------------------------------------------------------------------------------
# Confidence Intervals of the averaged periodogram
# ------------------------------------------------------------------------------

# df:  adjusted degrees of freedom = 2 * L * 160 / 160 ~ 18

( df <- Ud.ave$df )




# ----------
# Confidence Intervals

U <- qchisq(0.025, df)

L <- qchisq(0.975, df)

df * soi.ave$spec[14] / L

df * soi.ave$spec[14] / U

