setwd("//media//kswada//MyFiles//R//geissler")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Geissler
#  - The complete data from Geissler's (1889) tabulation of family sex composition in Saxony.
# ------------------------------------------------------------------------------
data("Geissler", package = "vcdExtra")

data <- Geissler

str(data)


# extract only boys in family with size == 11
data <- data %>% filter(size == 11) %>% dplyr::select(boys, Freq)
data <- xtabs(Freq ~ boys, data = data)



# ------------------------------------------------------------------------------
# Ord plot for Federalist Papers data
#
# Ord plot may be used to diagnose the form of the discrete distribution.
# Ord showed that a linear relationship:  the ratios k * n(k) / n(k-1) plotted against k should appear as a straight line,
# whose slope and intercept determin the particular distribution
#
#  slope = 0,  Intercept = + : Posisson
#  slope = -,  Intercept = + : Binomial
#  slope = +,  Intercept = + : Negative binomial
#  slope = +,  Intercept = - : Log. series
#
# black line shows the usual ordinary least squares (OLS) regression fit of frequency, n(k), on number of occurrences, k
# red line shows the weighted least sqaures fit, using weights of sqrt(n(k) - 1)
# ------------------------------------------------------------------------------
Ord_plot(data, main = "Numbers of male children", gp = gpar(cex = 1), pch = 16)


# -->
# The negative slope and positive intercept clearly diagnose this distribution as binomial.
