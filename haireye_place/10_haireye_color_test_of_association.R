setwd("//media//kswada//MyFiles//R//haireye_color")

packages <- c("dplyr", "datasets", "vcd")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  HairEyeColor
#  - frequencies of hair color, eye color, and sex from 592 students in a statistical course (Snee's 1974 sample)
# ------------------------------------------------------------------------------
data("HairEyeColor", package = "datasets")

dim(HairEyeColor)
str(HairEyeColor)
dimnames(HairEyeColor)

HairEyeColor



# ------------------------------------------------------------------------------
# Overall tests of association
#  - The measures of association are normalized variants of the chi-square statistic.
#    Caution is needed for interpretation since the maximum values depend on the table dimensions
# ------------------------------------------------------------------------------
( hec <- margin.table(HairEyeColor, 2:1) )

vcd::assocstats(hec)


# --> Pearson X^2 of 138.3 with 9 df, indicating substantial departure from independence
# Cramer's V = sqrt(X^2 / (N * min(r-1, c-1))) = sqrt(138.29 / (592 * 3)) = 0.279, indicates a substantial relationship between hair and eye color.

