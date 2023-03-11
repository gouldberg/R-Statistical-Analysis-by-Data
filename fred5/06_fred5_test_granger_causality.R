# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/fred5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# --> Continued from previous scripts

# ------------------------------------------------------------------------------
# Granger causality testing by vars::causality()
# ------------------------------------------------------------------------------

# Granger causality H0:  Dc do not Granger-cause Dy --> rejected
# H0: No instantaneous causality between Dc and Dy --> rejected
vars::causality(varfit, cause = "Dc")



# ----------
# Granger causality H0:  Dy do not Granger-cause Dc --> NOT rejected
# H0: No instantaneous causality between Dc and Dy --> rejected
vars::causality(varfit, cause = "Dy")



# -->
# Dc causes granger causality to Dy
# but Dy does not cause granger causality to Dc



# ------------------------------------------------------------------------------
# Granger causality testing by lmtest::grangertest()
# ------------------------------------------------------------------------------
# if not VAR
library(lmtest)


lmtest::grangertest(Dy ~ Dc, order = 3, data_dif)  # --> rejected

lmtest::grangertest(Dc ~ Dy, order = 3, data_dif)  # --> NOT rejected

