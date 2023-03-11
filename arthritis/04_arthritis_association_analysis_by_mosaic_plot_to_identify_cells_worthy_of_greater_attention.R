setwd("//media//kswada//MyFiles//R//arthritis")

packages <- c("dplyr", "vcd", "MASS", "datasets")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arthritis
# ------------------------------------------------------------------------------
data("Arthritis", package = "vcd")

data <- Arthritis

data


# ----------
# We use only (larger) female patient group
tab2 <- xtabs(~ Treatment + Improved, data = data, subset = Sex == "Female")


names(dimnames(tab2))[2] <- "Improvement"
tab2



# ------------------------------------------------------------------------------
# masaic plot by shading_Friendly
#  - An overall test for association using Pearson' X^2 may not be significant
# ------------------------------------------------------------------------------

# margin() to plot is larger a little bit
# This data set is somewhat paradoxical, in that the standard chisq.test() for association with these data gives
# a highly significant result, X^2 = 11.3, p = 0.0035, while the shading pattern using shading_Friendly shows all residuals within +-2, and thus unshaded
mosaic(tab2, gp = shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digits = 2)

chisq.test(tab2)



# ------------------------------------------------------------------------------
# masaic plot by shaindg_max
#  - the maximum residual test may highlight one or more cells worthy of greater attension
# ------------------------------------------------------------------------------
# shading levels determined by significant maximum residuals via shading_max
# This shows that significant deviations from independence occur in the cour corner cells, corresponding to more of the 
# treated group showing marked improvement, and more of the placebo group showing no improvement
mosaic(tab2, gp = shading_max, margin = c(right = 1))



# ----------
# how shading_max function:

# Pearson residuals for this table
residuals(chisq.test(tab2))


# The shading_max() function then calls coindep_test() to generate n = 1000 random tables with the same margins,
# and computes the maximum residual statistic for each
# This gives a non-parametric p-value for the test of independence, p = 0.011
set.seed(1234)

tab2_max <- coindep_test(tab2)

tab2_max


# Finally, the 0.90 and 0.99 quantiles of the simulation distribution are used as shading levels,
# passed as the value of the interpolate argument
tab2_max$qdist(c(0.90, 0.99))




