setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)


inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)



# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------

graphics.off()

forecast::Ccf(prec, inf, lag = 24, main = "Sqrt Precipitation vs. Logged Inflow")




# The dashed lines shown on the plots indicate +- 2 / sqrt(454) = +- 2 / sqrt(n);
# large sample distribution of cross-correlation is normal with mean zero and standard deviation = 1 / sqrt(n) if at least one of the processes is
# independent white noise.

# But since neither series is noise, these lines do not apply.

# in order for the dashed lines to be significant, at least one of the series must be white noise
# If this is not the case, there is no simple way to tell if a cross-correlation estimate is significantly different from zero
# We are only guessing at the linear dependence relationship between sqrt precipitation and logged inflow




# ----------
astsa::lag2.plot(prec, inf, max.lag = 24)




# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------

acf(climhyd)
