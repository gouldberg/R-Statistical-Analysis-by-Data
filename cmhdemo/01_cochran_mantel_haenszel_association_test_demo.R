setwd("//media//kswada//MyFiles//R//cmhdemo")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  demo data
# ------------------------------------------------------------------------------
cmhdemo1 <- read.table(header=TRUE, sep="", text="
     b1  b2   b3  b4  b5
a1    0  15   25  15   0
a2    5  20    5  20   5
a3   20   5    5   5  20
")

( cmhdemo1 <- as.matrix(cmhdemo1) )


# linear association
cmhdemo2 <- read.table(header=TRUE, sep="", text="
     b1  b2   b3  b4  b5
a1    2   5    8   8   8
a2    2   8    8   8   5
a3    5   8    8   8   2
a4    8   8    8   5   2
")

( cmhdemo2 <- as.matrix(cmhdemo2) )



# ------------------------------------------------------------------------------
# Standard chisq test
# ------------------------------------------------------------------------------
assocstats(cmhdemo1)


# ----------
# linear association data does not reject the independence hypothesis and have low Cramer's V and 
assocstats(cmhdemo2)



# ------------------------------------------------------------------------------
# Sieve diagram
# ------------------------------------------------------------------------------
sieve(cmhdemo1, shade=TRUE, main="General association",
      gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))


# ----------
# sieve diagram show slightly weak linear association pattern
sieve(cmhdemo2, shade=TRUE, main="Linear association",
      gp = shading_sieve(interpolate = 0, lty = c("solid", "longdash")))



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

# The chi-square values for non-zero correlation and difference row mean scores are exactly zero because the row means are all equal.
# Only the general associaion test shows that A and B are associated.
CMHtest(cmhdemo1)



# ----------
# Note that the X^2 values for the row-means and non-zero correlation test from CMHtest() are very similar.
# But the correlation test is more highly significant since it is based on just one degree of freedom.
CMHtest(cmhdemo2)






