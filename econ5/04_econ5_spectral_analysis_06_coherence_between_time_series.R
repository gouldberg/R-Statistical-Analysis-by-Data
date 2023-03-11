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




# ------------------------------------------------------------------------------
# Spectral analysis:  Coherence between economic indicators
#   - Coherence:  correlation indexed by frequency
#   - Squared coherence:
#        An important example of the application of the cross-spectrum is to the problem of predicting an output series y(t) from
#        some input series x(t) through a linear filter relation such as the three-point moving average.
#        A measure of the strength of such a relation is the squared coherence.
# ------------------------------------------------------------------------------

Ud <- diff(econ5$unemp)

Gd <- diff(econ5$govinv)

Cd <- diff(econ5$consum)



dat <- cbind(Gd, Ud, Cd)


# Display all the squared coherencies suppressing the confidence intervals
sr <- mvspec(dat, spans = c(7, 7), taper = 0.5, plot.type = "coh", ci = -1)


# -->
# suggests a lagged regression relation between the two series.




# ------------------------------------------------------------------------------
# Spectral analysis:  Coherence between economic indicators
#   - de-trended series
# ------------------------------------------------------------------------------

dat <- cbind(G, U, C)


sr <- mvspec(dat, spans = c(7, 7), taper = 0.5, plot.type = "coh", ci = -1)




# ----------
# We used L = 19
L <- 19

( m <- (L - 1)/2 )

sr2 <- mvspec(dat, kernel("daniell", m), plot.type = "coh", ci = -1)



par(mfrow = c(1,1))

plot(sr2)




# ----------
# df = 2 * L * 161 / 162 =~ 38,  F(2,df-2)(.0.001) ~ 8.98
sr$df





# ----------
( f <- qf(p = .999, df1 = 2, df2 = sr$df - 2) )

( C <- f / (L - 1 + f) )


par(mfrow = c(1,1))


plot(sr2, plot.type = "coh", ci.lty = 2)

abline(h = C)




# -->
# We may reject the hypothesis of no coherence for values of rho(y*x)^2(omega) that exceed C(0.001) = C


