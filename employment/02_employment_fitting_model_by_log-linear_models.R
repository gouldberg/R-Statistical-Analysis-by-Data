setwd("//media//kswada//MyFiles//R//employment")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Employment status
# ------------------------------------------------------------------------------
data("Employment", package = "vcd")

data <- Employment

data




# ------------------------------------------------------------------------------
# EmploymentStatus as the response variable, and EmploymentLength and LayoffCause as predictors
#
# Baseline model:  [EmploymentLength * LayoffCause][EmploymentStatus]
#  --> employment status is independent of both length and cause ?
# ------------------------------------------------------------------------------
# This baseline model fits quite poorly

mosaic(data, shade = TRUE, expected = ~ EmploymentLength * LayoffCause + EmploymentStatus, main = "Length * Cause + EmploymentStatus")


mod1 <- loglm(~ EmploymentLength * LayoffCause + EmploymentStatus, data = data)

mod1


# --> The residuals indicate an opposit pattern for the two categories of LayoffCause.
# Those who were laid off as aresult of a closure are more likely to be unemployed, regardless of length of time they were employed.
# Workers who were replaced, however, apparently are more likely to be employed, particularly if they were employed for 3 months or more.



# ------------------------------------------------------------------------------
# Conditional independence model:  [EmploymentLength * LayoffCause][EmploymentStatus * LayoffCause]
# ------------------------------------------------------------------------------
# Beyond this baselne model, it is substantively more meaningful to consider the conditional independence model,
# which asserts that employment status is independent of length of employment, given the cause of layoff.

mod2 <- loglm(~ EmploymentLength * LayoffCause + EmploymentStatus * LayoffCause, data = data)

anova(mod1, mod2, test = "chisq") 


mosaic(data, shade = TRUE, expected = ~ EmploymentLength * LayoffCause + EmploymentStatus * LayoffCause, main = "Length * Cause + Status * Cause",
       gp_args = list(interpolate = 1:4))


# -->
# This model fits far better G^2(10) = 24.63, but the lack of fit is still significant.
# Residuals still suggest that the pattern of association between employment and length is difficult
# for replaced workers and those laid off due to closure of their workplace.

