setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
# ------------------------------------------------------------------------------

data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)



# ----------
# set the lowest category to zero in order to make it eRm compaatible
itceaq <- CEAQ[,1:16] - 1



# ----------
library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho




# ------------------------------------------------------------------------------
# Assess parameter distribution
# ------------------------------------------------------------------------------

coef_df <- data.frame(coef(fitrsm2)) %>% mutate(item = row.names(.), c1_2 = as.factor(rep(c(1,2), 14)))


coef_df



# ----------
par(mfrow = c(1,1))
car::densityPlot(coef_df$coef.fitrsm2., g = coef_df$c1_2, legend = FALSE, lty = 1:2, col = c("black", "blue"))



# -->
# Note that category 0 is referenced level
# the distribution of c2 is wider




