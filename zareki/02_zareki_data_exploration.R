setwd("//media//kswada//MyFiles//R//zareki")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# We consider eight binary subtraction items only
zarsub <- zareki[, grep("subtr", colnames(zareki))]

str(zarsub)




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


summary(zareki)



# ----------
table(zareki$class)


xtabs(~ class + time, data = zareki)




# ----------
# min time required is smaller upper class
boxplot(time ~ class, data = zareki)