setwd("//media//kswada//MyFiles//R//gorf")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf and gorm
# ------------------------------------------------------------------------------

dim(gorf.dat)

dim(gorm.dat)




# ------------------------------------------------------------------------------
# Kendall Coordinates
#   - invariant to location, orientation and scaling
# ------------------------------------------------------------------------------

test <- gorf.dat[,,5]



# ---------
# Kendall coordinates
( test_kendall <- kendall2d(test) )



# ----------
graphics.off()

par(mfrow=c(1,2))
plot(test, axes = F, xlab = "", ylab = "")
polygon(test)

plot(test_kendall, axes = F, xlab = "", ylab = "")
polygon(test_kendall)
