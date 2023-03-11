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
# Spectral analysis:  Coherence between all the weather variables and transformed inflow
#   - Coherence:  correlation indexed by frequency
#   - Squared coherence:
#        An important example of the application of the cross-spectrum is to the problem of predicting an output series y(t) from
#        some input series x(t) through a linear filter relation such as the three-point moving average.
#        A measure of the strength of such a relation is the squared coherence.
# ------------------------------------------------------------------------------


dat <- cbind(climhyd, inf, prec)


# Display all the squared coherencies suppressing the confidence intervals

sr <- mvspec(dat, spans = c(7, 7), taper = 0.5, plot.type = "coh", ci = -1)



# -->
# high coherence between sqrt precipitation and logged inflow series
# suggests a lagged regression relation between the two series.




# ----------
L <- 19

( m <- (L - 1)/2 )

sr2 <- mvspec(dat, kernel("daniell", m), plot.type = "coherency", ci = -1)




# ----------
# df = 2 * L * 454 / 480 =~ 36,  F(2,df-2)(.0.001) ~ 8.98
sr$df


