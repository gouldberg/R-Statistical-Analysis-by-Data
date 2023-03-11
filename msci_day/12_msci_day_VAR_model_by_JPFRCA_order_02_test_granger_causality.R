# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R/msci_day")


packages <- c("dplyr", "AER", "stargazer", "broom", "knitr", "tseries", "vars", "MTS", "forecast")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# --> Continued from previous scripts

# ------------------------------------------------------------------------------
# Granger causality testing by vars::causality()
# ------------------------------------------------------------------------------

# Granger causality H0:  jp do not Granger-cause fr ca --> marginally rejected
# H0: No instantaneous causality between jp and fr ca --> rejected
vars::causality(varfit, cause = "jp")


# NOTE THAT F-statistics is 0.2942 * 3 = 0.8826 in the background



# ----------
# Granger causality H0:  fr do not Granger-cause jp ca --> rejected
# H0: No instantaneous causality between fr and jp ca --> rejected
vars::causality(varfit, cause = "fr")


# NOTE THAT F-statistics is 22.248 * 3 = 66.744 in the background



# ----------
# Granger causality H0:  us do not Granger-cause jp fr --> rejected
# H0: No instantaneous causality between us and jp fr --> rejected
vars::causality(varfit, cause = "ca")


# NOTE THAT F-statistics is 18.44 * 3 = 55.32 in the background



# ------------------------------------------------------------------------------
# Granger causality testing by lmtest::grangertest()
# ------------------------------------------------------------------------------
# if not VAR
library(lmtest)


# NOTE THAT F-statistics is multuplied by 3 in the background

lmtest::grangertest(jp ~ fr, order = 3, data_dif)  # --> rejected

lmtest::grangertest(jp ~ ca, order = 3, data_dif)  # --> rejected


# -->
# Both FR and CA daily stock index return (%) is useful to predict JP daily stock index return (%)


lmtest::grangertest(fr ~ jp, order = 3, data_dif)  # --> NOT rejected

lmtest::grangertest(fr ~ ca, order = 3, data_dif)  # --> rejected


# -->
# For FR, only CA is useful



lmtest::grangertest(ca ~ jp, order = 3, data_dif)  # --> NOT rejected

lmtest::grangertest(ca ~ fr, order = 3, data_dif)  # --> NOT rejected


# -->
# JP --> FR --> CA --> JP --> FR --> CA

# Granger cause:
#  - FR --> JP
#  - CA --> JP
#  - CA --> FR

# Granger do not cause:
#  - JP --> FR
#  - JP --> CA
#  - FR --> CA

