setwd("//media//kswada//MyFiles//R//arthritis")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arthritis
# ------------------------------------------------------------------------------
data("Arthritis", package = "vcd")

data <- Arthritis

data



# ------------------------------------------------------------------------------
# Measures of association:  "Improved" is different by "Treatment" ?
# ------------------------------------------------------------------------------
tab <- xtabs(~ Treatment + Improved, data = data)

tab



# ----------
# "Imptoved" is different by "Treatment" ?
# For those give the placebo, 67% reported no improvement
# In the treated group, 51% reported marked improvement
round(100 * prop.table(tab, margin=1), digits=2)



# ----------
# X^2 tests and measures of association (normalized variants of the X^2 statistic)
# Caution is needed for interpretation since the maximum values depend on the table dimension.
# Cramer's V = 0.394
assocstats(tab)



# ------------------------------------------------------------------------------
# Measures of association:  computing strata-wise statistics
#
# contorolling for gender
# ------------------------------------------------------------------------------
tab2 <- xtabs(~ Treatment + Improved + Sex, data = data)

tab2



# ----------
assocstats(tab2)


# --> Note that even though the strength of association (Cramer's V) is similar in the two groups (female and male)
# the X^2 tests show significance for females, but not for males.


# ----------
CMHtest(tab2)


# --> This is true even using the more powerful CMH tests below, treating Treatment as ordinal.
# The reason is that there were more than twice as many females as males in this sample.


apply(tab2, 3, sum)




