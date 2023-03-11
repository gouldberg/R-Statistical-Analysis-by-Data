setwd("//media//kswada//MyFiles//R//powerplant")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS", "timsac")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Power Plant
#   - 500 observations on 3 variables:  comamnd, temperature and fuel
# ------------------------------------------------------------------------------

data(Powerplant, package = "timsac")


dim(Powerplant)


str(Powerplant)


head(Powerplant)


colnames(Powerplant) <- c("command", "temperature", "fuel")




# ------------------------------------------------------------------------------
# Spectral analysis:  Coherence between variables
#   - Coherence:  correlation indexed by frequency
#   - Squared coherence:
#        An important example of the application of the cross-spectrum is to the problem of predicting an output series y(t) from
#        some input series x(t) through a linear filter relation such as the three-point moving average.
#        A measure of the strength of such a relation is the squared coherence.
# ------------------------------------------------------------------------------


# Display all the squared coherencies suppressing the confidence intervals

sr <- mvspec(Powerplant, spans = c(7, 7), taper = 0.25, plot.type = "coh", ci = -1)



# -->
# high coherence between sqrt precipitation and logged inflow series
# suggests a lagged regression relation between the two series.




# ----------
L <- 9

( m <- (L - 1)/2 )

sr2 <- mvspec(Powerplant, kernel("daniell", m), plot.type = "coh", ci = -1)



# ----------
sr$df


