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
# Assess homogeneity of association (homogeneity of odds ratios)
# ------------------------------------------------------------------------------


woolf_test(margin.table(tab,c(1,2,3)))


woolf_test(margin.table(tab,c(1,2,4)))




# -->
# if the stratifying factors are single by Age or Educatoin (not jointed by Age and Education)
# association is NOT heterogeneity


