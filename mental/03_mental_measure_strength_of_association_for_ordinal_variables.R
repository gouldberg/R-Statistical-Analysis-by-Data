setwd("//media//kswada//MyFiles//R//mental")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Mental
# ------------------------------------------------------------------------------
data("Mental", package = "vcdExtra")

data <- Mental

data

tab <- xtabs(Freq ~ ses + mental, data = Mental)

tab



# ------------------------------------------------------------------------------
# Standard chisq test
# ------------------------------------------------------------------------------
assocstats(tab)



# ------------------------------------------------------------------------------
# Generalized Cochran-Mantel-Haenszel tests
#  - take the ordinal nature of a variable into account
#  - based on assigining numerical scores to the table categories; the default (table) scores treat the levels as equally spaced.
#    They generally have higher power when the pattern of association is determined by the order of an ordinal variable
#
#
#  - General Association:  When the row and column variables are both nominal (unordered), the only alternative hypothesis of interest is that
#    there is some association between the row and column varibales. The CMH test statistic is similar to the (Pearson) Chi-Square and Likelihood Ratio Chi-Square
#    in the result from assocstats(); all have (r-1)(c-1) df.
#
#  - Row Means Scores Differ:  If the column variable is ordinal, assigning scores to the column vaariable produces a mean of each row.
#    The asoociation between row and column variables can be expressed as a test of whether these means differ over the rows of the table, with r -1 df.
#    This is anologous tothe Kruskal-Wallis non-parametric test (ANOVA based on rank scores)
#
#  - Column Mean Scores Differ:  Same as the above, assigning scores to the row variable
#
#  - Nonzero Correlation (Linear association):  When both row and column variables are ordinal, we could assign scores to both variables and compute the correlation (r),
#    giving Spearmans's rank correlation coefficient. The CMH Chi-square is equal to (N-1)r^2, where N is the total sample size.
#    The test is most sensitive to a pattern where the row mean score changes linearly over the rows.
# ------------------------------------------------------------------------------
# All four tests show a highly significant association.
# However the cor test for nonzero correlation uses only one degree of freedom, whereas the test of general association requires 15 df.

CMHtest(tab)



