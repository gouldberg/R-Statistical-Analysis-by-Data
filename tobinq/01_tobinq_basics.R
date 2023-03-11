setwd("//media//kswada//MyFiles//R//tobinq")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TobinQ
#  - Data concern 188 American firms for 35 years (from 1951 to 1995)
#  - Schaller (1990) wishes to test Tobin (1969)'s theory of investment.
#    In this model, the main variable that explains investment is the ratio between the value of the firm and the replacement cost of its physical capital,
#    this ratio being called "Tobin Q".
#    If the financial market is perfect, the value of the firm equals the actual value of its future profits.
#    If the Tobin Q is greater than 1, this means that the profitability of investment is greater than its coast and so that the investment is valuable.
# ------------------------------------------------------------------------------

data("TobinQ", package = "pder")

str(TobinQ)

dim(TobinQ)



# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------


