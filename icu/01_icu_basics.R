setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
#   - A classic study by Lemeshow et al. (1988) of patients admitted to an intensive care unit at Baystate medical Center in Springfield, Massachusetts.
#   - The major goal of this study was to develop a model to predict the probability of survival (until hospital discharge) of these patients and to
#     study the risk factors associated with ICU mortality.
#   - The data gives the results for a sample of 200 patients that was presented in Hosmer et al. (2013) (and ealier editions).
#     The data contains 22 variables of which the first, died, is a factor. Among the predictors, two variables (race, coma) were represented initially as
#     3-levlel factors, but then recoded to binary variables (white, uncons).
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ------------------------------------------------------------------------------
# basic analysis
# ------------------------------------------------------------------------------


# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables
data <- data[, -c(4, 20)]
str(data)

Hmisc::describe(data)

psych::describe(data)
