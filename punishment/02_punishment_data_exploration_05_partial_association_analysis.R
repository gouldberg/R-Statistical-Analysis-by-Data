setwd("//media//kswada//MyFiles//R//punishment")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Punishment
# ------------------------------------------------------------------------------

data("Punishment", package = "vcd")


data <- Punishment


data


str(Punishment)



# ----------
# convert data.frame to table

tab <- xtabs(Freq ~ memory + attitude + age + education, data = data)


dimnames(tab) <- list(
  Memory = c("yes", "no"),
  Attitude = c("no", "moderate"),
  Age = c("15-24", "25-39", "40+"),
  Education = c("Elementary", "Secondary", "High")
)



# Memory * Attitude * Age * Education
tab




# ------------------------------------------------------------------------------
# Overall Test of Conditional Independence
# 
# Of main interest here is the association between
#   - (A) attitude toward corporal punishment as an adult
#   - (B) memory of corporal punishment as a child
#   controlling for age (C) and education (D) --> [ACD][BCD]
# ------------------------------------------------------------------------------


# loglm()
mod.cond <- loglm(~ Memory * Age * Education + Attitude * Age * Education, data = tab)


# Pearson X^2 = 34.604
mod.cond




# ----------
# Alternatively, coindep_test() provides tests of conditional independence of two variables
# in a contingency table by simulation from the marginal permutation distribution of the input table,
# reporting a Pearson X^2 statistic

set.seed(1071)

coindep_test(tab, margin = c("Age", "Education"), indepfun = function(x) sum(x^2), aggfun = sum)



# -->
# These tests all show substantial association between attitude and memory of corporal punishment




# ------------------------------------------------------------------------------
# The divide-and-conquer strategy of partial association
#  - partition the overall G^2 or X^2 to show the contribution to this association from the combinations of age and education
# ------------------------------------------------------------------------------

mods.list <- apply(tab, c("Age", "Education"), function(x) loglm(~ Memory + Attitude, data = x)$pearson)


# Pearson X^2 is decomposed (total is 34.604)
mods.list
sum(mods.list)



# --> Pearson X^2 shows that the association between attitude and memory becomes stronger with increasing age among those with the lowest education.
# Among those in the highest age group, the strength of association decreases with increasing education.



# ----------
# Visual analog of above analysis
set.seed(1071)

pun_cotab <- cotab_coindep(tab, condvars = 3:4, type = "mosaic", varnames = FALSE, margins = c(2,1,1,2), test = "sumchisq", interpolate = 1:2)

cotabplot(~ Memory + Attitude | Age + Education, data = tab, panel = pun_cotab)




# ----------
# mosaic plot shows also the relative sizes of the tiles for the combinations of age and education
mosaic(~ Memory + Attitude | Age + Education, data = tab, shade = TRUE, gp_args = list(interpolate = 1:4))



# --> Conclusion
# memory of experienced violence as a child tends to engender a more favorable attitude toward corporal punishment as an adult,
# but this association varies directly with both age nad education.

