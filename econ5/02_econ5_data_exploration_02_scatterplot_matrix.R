setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")


str(econ5)


car::some(econ5)




# ------------------------------------------------------------------------------
# data exploration:  scatterplot matrix
# ------------------------------------------------------------------------------


psych::pairs.panels(econ5)


psych::pairs.panels(mapply(FUN = diff, econ5))





# ----------

formula <- ~ diff(unemp) + diff(gnp) + diff(consum) + diff(govinv) + diff(prinv)


car::scatterplotMatrix(formula, data = econ5,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)
