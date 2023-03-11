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
# Estimate difference by joint regression, add more variables
# ------------------------------------------------------------------------------

mod1 <- lm(fte ~ nj * d, data = njmin3)


mod2 <- lm(fte ~ nj * d + kfc + roys + wendys + co_owned, data = njmin3)


mod3 <- lm(fte ~ nj * d + kfc + roys + wendys + co_owned + southj + centralj + pa1, data = njmin3)



# ----------
library(stargazer)

stargazer(mod1, mod2, mod3, type = "text", keep.stat = "n", digits = 2, single.row = TRUE, intercept.bottom = FALSE)



# -->
# Here including more variables,
# DiD estimator is
#  mod1: 2.754   mod2: 2.845   mod3: 2.815

# kfc and southj has large negative coefficient.




# ------------
# log version
mod1 <- lm(log(fte2) ~ nj * d, data = tmp)

mod2 <- lm(log(fte2) ~ nj * d + kfc + roys + wendys + co_owned, data = tmp)

mod3 <- lm(log(fte2) ~ nj * d + kfc + roys + wendys + co_owned + southj + centralj + pa1, data = tmp)


library(stargazer)

stargazer(mod1, mod2, mod3, type = "text", keep.stat = "n", digits = 2, single.row = TRUE, intercept.bottom = FALSE)



# -->
# law change of minimum wage has effect of 6.0% increase of full-time equivalent employment

