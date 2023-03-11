setwd("//media//kswada//MyFiles//R//flu")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  flu
# ------------------------------------------------------------------------------

data(flu, package = "astsa")

str(flu)

flu



# ------------------------------------------------------------------------------
# correlation analysis
# ------------------------------------------------------------------------------


acf2(flu)


acf2(diff(flu))



# -->
# strong yearly auto-correlation



# ----------
sarima(flu, p = 1, d = 0, q = 1, P = 0, D = 0, Q = 1, S = 12)
