# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arthritis
#  - Data from Koch and Edwards (1988), representing a double-blind clinical trial investigating a new treatment for rheumatoid arthritis with 84 patients.
# ------------------------------------------------------------------------------
data("Arthritis", package = "vcd")

dim(Arthritis)
str(Arthritis)


car::some(Arthritis)



# ------------------------------------------------------------------------------
# Overview by table
# ------------------------------------------------------------------------------
# Case form to table form
Art.tab <- table(Arthritis[,c("Treatment", "Sex", "Improved")])
str(Art.tab)


ftable(Art.tab)
ftable(Art.tab, row.vars = c("Sex","Treatment"))
round(prop.table(ftable(Art.tab, row.vars = c("Sex","Treatment")),1), digits = 3)



# ----------
# table form to case form
expand.dft(Art.tab)



# ------------------------------------------------------------------------------
# Overview of the data:  gpairs
# ------------------------------------------------------------------------------
gpairs::gpairs(Arthritis[,c(5,2,3,4)], diag.pars = list(fontsize = 20), mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate = 1:4)))


# --> The bottom row, corresponding to Age, uses boxplots to show the distribution of age for each of the categorical variables.
# The last column shows these same variables as stripplots (or "barcodes"), which show all the individual observations.
# In the (1,4) and (4,1) panels, it can be seen that younger patietns are more likely to report no improvement.
# The other panels in the first row (and column) show that improvement is more likely in the treated condition and greater among women than men.



# ------------------------------------------------------------------------------
# 2-way table
# ------------------------------------------------------------------------------
( Art <- xtabs(~ Treatment + Improved, data = Arthritis) )


round(100 * prop.table(Art, margin = 1), 2)



# ------------------------------------------------------------------------------
# spline plot:  a stacked barchart of the row percentages
# ------------------------------------------------------------------------------
spineplot(Art)



# ------------------------------------------------------------------------------
# Overall tests of association
#  - The measures of association are normalized variants of the chi-square statistic.
#    Caution is needed for interpretation since the maximum values depend on the table dimensions
# ------------------------------------------------------------------------------
vcd::assocstats(Art)



# ------------------------------------------------------------------------------
# Tests for ordinal variables
#  - Generalized Cochran-Mantel-Haenszel tests
#  - These tests are based on assigning numerical scores to the table categories; the default (table) scores treat the levels as equally spaced.
#  - They generally have higher power when the pattern of association is determined by the order of an ordinal variable
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
# All four tests show a highly significant assocation.
# However, the cor test for nonzero correlation uses only one degree of freedom, whereas the test of general association requires 2 df.
Art
vcdExtra::CMHtest(Art)



# ------------------------------------------------------------------------------
# Computing strata-wise statistics:  3-way table
#  - Stratified analysis of the arthritis treatment, controlling for gender
# ------------------------------------------------------------------------------
( Art2 <- xtabs(~ Treatment + Improved + Sex, data = Arthritis) )


# doubldecker plot
vcd::doubledecker(Improved ~ Treatment + Sex, data = Arthritis)
vcd::doubledecker(Improved ~ Sex + Treatment, data = Arthritis)


# mosaic display
vcd::mosaic(xtabs(~ Sex + Treatment + Improved, data = Arthritis), gp = shading_Friendly)



# ----------
# Even though the strength of association (Cramer's V) is similar in the two groups, the Chi-square tests show significance for females, but not for males.
vcd::assocstats(Art2)


# This is true even using the more powerful CMH tests, treating Treatment as ordinal
# The reason is that there were more than twice as many females as males in this sample.
vcdExtra::CMHtest(Art2)



# ------------------------------------------------------------------------------
# Assessing homogeneity of association
#  - Homogeneity means the association between treatment and outcome (improve) is the same for both men and women
# ------------------------------------------------------------------------------
# Even though we found in the CMH analysis above that the associaion between Treatment and Improved was stronger for females than males,
# the analysis using woolf_test() is clearly non-significant, so we cannot reject homogeneity of association.
vcd::woolf_test(Art2)



# ----------
# In the case of a 3-way table, the hypothesis of homeogeneity of association among 3 variables A, B, C can be stated as the loglinear model of no 3-way association,
# [AB][AC][BC].
# This hypothesis can be stated as the loglinear model, [SexTreatment][SexImproved][TreatmentImproved]
# Consistent with the Woolf test, the interaction terms are not significant.
MASS::loglm(~ (Treatment + Improved + Sex)^2, data = Art2)



# ------------------------------------------------------------------------------
# Mosaic Displays for Females
# ------------------------------------------------------------------------------
( Art_f <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female") )
names(dimnames(Art_f))[2] <- "Improvement"


# Fixed shadiing levels via shading_Friendly
vcd::mosaic(Art_f, gp = shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digits = 2)


# Chi-square test and Pearson residulas
chisq.test(Art_f)
residuals(chisq.test(Art_f))


# --> This data is somewhat paradoxical, in that the standard chisq.test() for associaion gives highly significant result,
# while the shading pattern shows all residuals within +-2 and thus unshaded



# ----------
# shading levels determined by significant maximum residuals via shading_max
# On the other hand, the shading_max shows that significant deviations from independence occur in the four corner cells,
# corresponding to more of the treated group showing marked improvement, and more of the placebo group showing no improvement.
set.seed(1234)
vcd::mosaic(Art_f, gp = shading_max, margin = c(right = 1))


# --> maximum residual test may highlight one or more cells worthy of greater attention.


# shading_max function calls coindep_test to generate n = 1000 random tables with the same margins, and computes the maximum residual statistic for each.
# This gives a non-parametric p-value for the test of independence, p = 0.011.
# the 0.90 and 0.99 quantiles of the simulation distribution are used as shading levels, passed as the value of the interpolate argument
( Art_f_max <- vcd::coindep_test(Art_f) )
Art_f_max$qdist(c(0.90, 0.99))





