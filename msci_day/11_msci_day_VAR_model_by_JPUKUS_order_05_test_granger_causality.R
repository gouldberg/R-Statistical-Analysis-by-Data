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

# Granger causality H0:  jp do not Granger-cause uk us --> NOT rejected
# H0: No instantaneous causality between jp and uk us --> rejected
vars::causality(varfit, cause = "jp")


# NOTE THAT F-statistics is 0.79756 * 3 = 2.39 in the background



# ----------
# Granger causality H0:  uk do not Granger-cause jp us --> rejected
# H0: No instantaneous causality between uk and jp us --> rejected
vars::causality(varfit, cause = "uk")


# NOTE THAT F-statistics is 6.4349 * 3 = 19.3047 in the background



# ----------
# Granger causality H0:  us do not Granger-cause jp uk --> rejected
# H0: No instantaneous causality between us and jp uk --> rejected
vars::causality(varfit, cause = "us")


# NOTE THAT F-statistics is 53.823 * 3 = 161.469 in the background



# ------------------------------------------------------------------------------
# Granger causality testing by lmtest::grangertest()
# ------------------------------------------------------------------------------
# if not VAR
library(lmtest)


# NOTE THAT F-statistics is multuplied by 3 in the background

lmtest::grangertest(jp ~ uk, order = 3, data_dif)  # --> rejected

lmtest::grangertest(jp ~ us, order = 3, data_dif)  # --> rejected


# -->
# Both UK and US daily stock index return (%) is useful to predict JP daily stock index return (%)


lmtest::grangertest(uk ~ jp, order = 3, data_dif)  # --> NOT rejected

lmtest::grangertest(uk ~ us, order = 3, data_dif)  # --> rejected


# -->
# For UK, only US is useful



lmtest::grangertest(us ~ jp, order = 3, data_dif)  # --> NOT rejected

lmtest::grangertest(us ~ uk, order = 3, data_dif)  # --> NOT rejected


# -->
# JP --> UK --> US --> JP --> UK --> US

# Granger cause:
#  - UK --> JP
#  - US --> JP
#  - US --> UK

# Granger do not cause:
#  - JP --> UK
#  - JP --> US
#  - UK --> US

