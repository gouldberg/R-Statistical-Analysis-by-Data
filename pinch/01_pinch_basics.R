setwd("//media//kswada//MyFiles//R//pinch")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pinch
#   - Data from some biomechanical data.  20 records of the force exerted on a meter during a brief pinch by the thumb and forefinger.
#     The subject was required to maintain a certain background force on a force meter and then to squeeze the meter aiming
#     at a specified maximum value, rerurning afterwards to the background lvel.
#   - The purpose of the experiment was to study the neurophysiology of the thumb-forefinger muscle group.
#     The data were collected at the MRC Applied Psychology Unit, Cambridge, by R. Flanagan
# ------------------------------------------------------------------------------

pinch <- matrix(scan("pinch.txt", 0), 151, 20, byrow=TRUE)

# data("pinch", package = "fda")

head(pinch)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

# sample records
idx <- sample(dim(pinch)[2], size = 4, replace = TRUE)


graphics.off()
par(mfrow=c(1,1))
matplot(pinch[, idx], type = "l", ylab = "Force (N)")



# Note that actual seconds range is 0 - 0.30
