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
# Fourfold display:  2 * 2
# ------------------------------------------------------------------------------

# unstandardized
# the area of each quadrant is proportional to the cell frequency, shown in numerically in each corner.
# The oods ratio is proportional to the product of the areas shaded dark, divided by the product of the areas shaded light
# here the no interpretation for confidence bands.

fourfold(margin.table(tab, c(1,2)), std = "ind.max")




# ------------------------------------------------------------------------------
# Fourfold display:  2 * 2  standardized
# ------------------------------------------------------------------------------


# standardize both margin
# confidence bands have interpretation as a test of H0: odds ratio = 1

fourfold(margin.table(tab, c(1,2)), fontsize = 30)




# ------------------------------------------------------------------------------
# Fourfold display:  2 * 2 * Age,  2 * 2 * Education
# ------------------------------------------------------------------------------


# by Age
fourfold(margin.table(tab, c(1,2,3)), fontsize = 30)



# by Education
fourfold(margin.table(tab, c(1,2,4)), fontsize = 30)




# ------------------------------------------------------------------------------
# Fourfold display:  2 * 2 * (Age * Education)
# ------------------------------------------------------------------------------


fourfold(margin.table(tab, c(1,2,3,4)), fontsize = 30)



# -->
# Education = Elementary  and Age = 40+   is strong association



