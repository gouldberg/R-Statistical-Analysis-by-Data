setwd("//media//kswada//MyFiles//R//db")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  db
# ------------------------------------------------------------------------------
data("db", package = "gamlss.data")


str(db)

car::some(db)



# ------------------------------------------------------------------------------
# plot fitted centile curves for y against x for different centile percentages
# ------------------------------------------------------------------------------

centiles(m0_c, db$age, legend = FALSE)


# -->
# Note that the sample percentage of observations below each of the fited centile curves from the fitted model
# are printed (at the end of each line,
# so comparisons with nominal model centiles (printed after below on each line)



# ----------
# modifid version
centiles(m0_c, db$age, cent = c(5, 25, 50, 75, 95), ylab = "head", xlab = "age", 
         col.centiles = c(2,3,1,3,2), lty.centiles = c(2,4,1,4,2), lwd.centiles = c(2,2,2.5,2,2))



# ------------------------------------------------------------------------------
# Calibration
#   - used when the fitted model centiles differ from the sample centiles
#     and it is assumed that this failure is the same for all values of the explanatory variable
#   - calibration() automatically adjusts the values selected for argument cent so that the sample percentage of cases below each centile curve is correct,
#     i.e. equal to the selected cent values
# ------------------------------------------------------------------------------

centiles(m0_c, db$age, cent = c(5, 25, 50, 75, 95))

calibration(m0_c, xvar = db$age, cent = c(5, 25, 50, 75, 95))



# ------------------------------------------------------------------------------
# Fan-chart of the centile curves
# ------------------------------------------------------------------------------

centiles.fan(m0_c, db$age, cent = c(5, 25, 50, 75, 95), ylab = "head", xlab = "age", colors = "gray")



# ------------------------------------------------------------------------------
# Split fitted centile curves according to different cut points in the x-variable
# ------------------------------------------------------------------------------

# cut at age 3
centiles.split(m0_c, xvar = db$age, xcut.points = c(3))



# number of intervals with equal numbers of cases
centiles.split(m0, xvar = dbbmi1$age, n.inter = 4)



