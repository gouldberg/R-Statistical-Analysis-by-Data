setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ------------------------------------------------------------------------------
# data exploration:  summary
# ------------------------------------------------------------------------------

summary(CodParasites)



# -->
# There are some missing value records.



# ----------
Hmisc::describe(CodParasites)


psych::describe(CodParasites)



# ----------
psych::describeBy(CodParasites$intensity, group = CodParasites$year)

psych::describeBy(CodParasites$intensity, group = CodParasites$area)

psych::describeBy(CodParasites[,c("year", "area", "intensity")], group = c("year", "area"))



