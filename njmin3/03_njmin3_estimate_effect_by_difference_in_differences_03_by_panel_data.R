setwd("//media//kswada//MyFiles//R//njmin3")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  njmin3
# ------------------------------------------------------------------------------

data("njmin3", package = "POE5Rdata")

dim(njmin3)

str(njmin3)


car::some(njmin3)



# ------------------------------------------------------------------------------
# Estimate difference with panel data
#   - In the data set njmin3, some restaurants, beloning to either group have been observed in both periods.
#     If we restrict the data set to only those restaurants we obtain a short (two period) panel data.
# ------------------------------------------------------------------------------


# we user demp (after-minus-before difference in employment)

mod3 <- lm(demp ~ nj, data = njmin3)


summary(mod3)


knitr::kable(tidy(summary(mod3)))


# -->
# estimated difference-in-differences coefficient is very close to the one we estimated before.
# Its t-statistic is still positive, indicating that the null hypothesis H0: "an increase in minimum wage increase employment" cannot be rejected.
