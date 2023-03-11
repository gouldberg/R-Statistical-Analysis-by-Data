# setwd("//media//kswada//MyFiles//R//divorce//")
# setwd("//media//kswada//MyFiles//R//Bayesian_inference//divorce//")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\divorce")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  divorce
#  - month lengths of living together ("doukyo") for each divorce case
#  - only 10,000 cases are randomly sampled without replace from data researched at 2014
#  - month lengths are 12 months interval --> 1 year (12 months) to 35 years (420 months)
#
#  - We assume:
#      - this is the data obtained from not retrospective but prospective study
#      - there are no truncations/withdrawals
#      - each month lengths data follows weibull distribution with same parameter  (no one weibull distributions for each month length)
#      - the number of marriages are same for each through 35 years
#      - there are no cohort effect by marriage period or age
# ------------------------------------------------------------------------------

source("divorce_data.R")

length(dat)

data.ls <- list(J = length(dat), tt = dat)

table(dat)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
