setwd("//media//kswada//MyFiles//R//soybean")

packages <- c("dplyr", "AppliedPredictiveModeling", "caret", "mlbench")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soybean
#   - The soybean data were collected to predict disease in 683 soybeans.
#     The 35 predictors are mostly categorical and include information on the environmental conditions
#     (e.g., temperature, precipitation) and plant conditions (e.g., left spots, mold growth).
#     The outcome labels consist of 19 distinct classes.
# ------------------------------------------------------------------------------
data("Soybean", package = "mlbench")

data <- Soybean

dim(data)

str(data)


