setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)



# ------------------------------------------------------------------------------
# basic analysis:  Stars plot
# ------------------------------------------------------------------------------

# Stars plot
# every observation is a "star" where the radius of a ray is proportional to a variable value
stars(x = wheat[order(wheat$type),-1], ncol = 20, key.loc = c(10, 0), draw.segments = TRUE, label = NULL, cex = 0.75)



