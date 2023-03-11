setwd("//media//kswada//MyFiles//R//kielmc")

packages <- c("dplyr", "foreign")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  kielmc
#   - Wooldridge (2016, Section 13.2) example:  effect of a garbage incinerator's location or housing prices
# ------------------------------------------------------------------------------

# library(foreign)
# kielmc <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/kielmc.dta")
# readr::write_tsv(kielmc, path = "kielmc.txt")

kielmc <- readr::read_tsv("kielmc.txt")


dim(kielmc)

str(kielmc)


car::some(kielmc)



# ------------------------------------------------------------------------------
# Estimate difference by joint regression, add more variables
# ------------------------------------------------------------------------------

DiD <- lm(lrprice ~ nearinc * y81, data = kielmc)


DiDContr <- lm(lprice ~ nearinc * y81 + age + I(age^2) + log(intst) + log(land) + log(area) + rooms + baths, data = kielmc)



# ----------
library(stargazer)

stargazer(DiD, DiDContr, type = "text", keep.stat = "n", digits = 2, single.row = TRUE, intercept.bottom = FALSE)



# -->
# Here, the model including more variables, the DiD estimator for nearinc:y81 is marginally statistically significant.
# in average, housing price is decreased by 13.2% !!!
