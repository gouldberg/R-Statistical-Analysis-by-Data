setwd("//media//kswada//MyFiles//R//fat_simulated")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fat
#  - created below, gives a 2 * 2 table recording the level of cholesterol in diet and the presence of symptoms of heart desease for a sample of 23 people.
# ------------------------------------------------------------------------------
fat <- matrix(c(6,4,2,11), 2, 2)
dimnames(fat) <- list(diet = c("LoChol", "HiChol"), disease = c("No", "Yes"))

fat


# -->  It seems that this data shows apparently strong association


# ------------------------------------------------------------------------------
# sieve diagram
# ------------------------------------------------------------------------------
sieve(fat, shade = TRUE)



# ------------------------------------------------------------------------------
# Test the association between diet and disease by chisq.test()
# ------------------------------------------------------------------------------
# The sample in each cell is too small.
# This test apply Yates' continuity correction but test may not be appropriate here.
chisq.test(fat)



# ------------------------------------------------------------------------------
# Visualize this association by fourfold()
# ------------------------------------------------------------------------------
fourfold(fat, std = "ind.max")
fourfold(fat, margin = 1)
fourfold(fat, margin = 2)


# ----------
# Apparently odds ratio differes significantly from 1
fourfold(fat)



# ------------------------------------------------------------------------------
# Odds ratio
# ------------------------------------------------------------------------------
# Odds ratio is 8.25, very large.  But the fourfold display does not show that much difference.
oddsratio(fat, log=FALSE)



# ------------------------------------------------------------------------------
# Fisher Exact Test may be more reliable for statistical inference with such a small sample
# ------------------------------------------------------------------------------
# sample estimate of odds ratio = 7.40 < 8.25
# lower limit of 95% confidence interval is 0.87 < 1, but p-value = 0.039 < 0.05
fisher.test(fat)









