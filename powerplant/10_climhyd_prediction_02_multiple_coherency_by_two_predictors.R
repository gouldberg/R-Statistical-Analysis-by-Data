setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)



# ------------------------------------------------------------------------------
# Multiple coherency
# ------------------------------------------------------------------------------

# We focus on the analysis with tow predictor series, temperature and transformed precipitation.


M <- 100


# Number of inputs (Temp and Precip)
nq <- 2



# ----------
# Multiple coherency
# stoch.reg:  frequency domain stochasitc regression  --> plot.which = "coh"
# 1: Temp   5: Precip

graphics.off()

coh.15 <- stoch.reg(Y, cols.full = c(1,5), cols.red = NULL, alpha, L, M, plot.which = "coh")

text(0.45, 0.98, plt.name[6], cex = 1.2)
title(main = c("Inflow with", "Temp and Precip"))



# -->
# The additional contribution of temperature to the model seems somewhat marginal
# because the multiple coherence seems only slightly better than the univariate coherence with precipitation.
