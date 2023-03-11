setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)



# ----------
# convert to fdata object
library(fda.usc)

# tension time series
tension1 <- as.matrix(tension[, 1:800])

cond <- tension$condition


ftension <- fdata(tension1, argvals = seq(1, 80, length.out = 800),
                  names = list(main = "Music tension", xlab = "Time (sec)", ylab = "Tension"))


ftension




# ------------------------------------------------------------------------------
#  Functional ANOVA:  whether there is an overall difference in the functional trajectories across the three conditions
#    - Here the response is functional and we have one categorical predictor (scalar) 
# ------------------------------------------------------------------------------

# We keep the number of bootstrap samples fairly low because the computation is quite time-consuming
set.seed(123)


# IT TAKES TIME !!!:  3 min.
# tension1way <- anova.onefactor(ftension1, cond, nboot = 50)
tension1way <- fanova.onefactor(ftension1, cond, nboot = 50)

tension1way

tension1way$pvalue



# -->
# The result suggests that there is an overall significant difference among the three smoothed mean curves.


