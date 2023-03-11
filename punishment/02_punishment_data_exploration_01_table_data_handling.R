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




# ------------------------------------------------------------------------------
# data exploration:  convert data.frame to table
# ------------------------------------------------------------------------------

tab <- xtabs(Freq ~ memory + attitude + age + education, data = data)


dimnames(tab) <- list(
  Memory = c("yes", "no"),
  Attitude = c("no", "moderate"),
  Age = c("15-24", "25-39", "40+"),
  Education = c("Elementary", "Secondary", "High")
)



# Memory * Attitude * Age * Education
tab


dim(tab)




# ------------------------------------------------------------------------------
# data exploration:  structable table
# ------------------------------------------------------------------------------


library(vcd)


structable(. ~ Education + Memory, data = tab)




# ------------------------------------------------------------------------------
# data exploration:  aperm
# ------------------------------------------------------------------------------


# Education * Age * Attitude * Memory

aperm(tab, c(4,3,2,1))




# ------------------------------------------------------------------------------
# data exploration:  margin table
# ------------------------------------------------------------------------------

margin.table(tab, margin = c(2,3))



