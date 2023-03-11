setwd("//media//kswada//MyFiles//R//gnp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  GNP data
#   - Quarterly U.S. GNP from 1947(1) to 2002(3), n = 223 observations
#     The data are real U.S. gross national product in billions of chained 1996 dollars and have seasonally adjusted.
#     The data were obtained from the Federal Reserve Bank of St. Louis.
# ------------------------------------------------------------------------------

data(gnp, package = "astsa")

str(gnp)

head(gnp)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

