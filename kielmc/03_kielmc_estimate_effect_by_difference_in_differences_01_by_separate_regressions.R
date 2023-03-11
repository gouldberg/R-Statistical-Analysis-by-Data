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
# Estimate difference by separate regressions
# ------------------------------------------------------------------------------

library(broom)


# Separate regressions for 1978 and 1981:  report coefficients only

kielmc %>% lm(data = ., rprice ~ nearinc, subset = (year == 1978)) %>% tidy()

kielmc %>% lm(data = ., rprice ~ nearinc, subset = (year == 1981)) %>% tidy()



# -->
# in 1981, the houses close to the construction site were cheaper by an average of 30,688 dollars,
# but this was not only due to the new incinerator since even in 1978, nearby houses were cheaper by an average of 18,824 dollars.

# The difference of these differences = 30688 - 18824 = 11864 dollars is the DiD (difference-in-differences) estimator
# and is arguably a better indicator of the actual effect.


