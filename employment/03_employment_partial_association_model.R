setwd("//media//kswada//MyFiles//R//employment")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Employment status
# ------------------------------------------------------------------------------
data("Employment", package = "vcd")

data <- Employment

data




# ------------------------------------------------------------------------------
# Partial association model
#  - partial association refers to the association among some variables within the levels of the other variables.
#  - partial association represents a useful "divide and conquer" statistical strategy
#     - it allows you to refine the question you want to answer for complex relations by breaking it down to smaller, easier questions.
#  - The overall likelihood ratio G^2 for the conditional independence model with (I-1)(J-1)K degrees of freedom is the sum of the values for the 
#    ordinary association between A and B over the levels of C (each with (I-1)(J-1) degrees of freedoms)
#    The same additive relationship holds for the Pearson X^2 statistics.
#  - Thus, (a) the overall G^2 (X^2) may be decomposed in to portions attributable to the AB association in the layers of C,
#    and (b) the collection of mosaic displays for the dependence of A and B for each of the levels of C provides a natural visualization of this decomposition.

#  - fit separate models for the partial relationship between EmploymentStatus and EmploymentLength for the two levels of LayoffCause.
# ------------------------------------------------------------------------------

# model separately by "LayoffCause"
mods.list <- apply(data, "LayoffCause", function(x) loglm(~ EmploymentStatus + EmploymentLength, data = x))

mods.list



# ----------
mosaic(data[,,"Closure"], shade = TRUE, gp_args = list(interpolate = 1:4), margin = c(right = 1), main = "Layoff: Closure")

mosaic(data[,,"Replaced"], shade = TRUE, gp_args = list(interpolate = 1:4), margin = c(right = 1), main = "Layoff: Replaced")



# --> Workers laid off due to closure of their company, length of previous employment is unrelated to whether or not they are re-employed
# However, for workers who were replaced, there is a systematic pattern: those who had been employed for three months or less are likely to remain unemployed,
# while those with longer job tenure are somewhat more likely to have found a new job.

