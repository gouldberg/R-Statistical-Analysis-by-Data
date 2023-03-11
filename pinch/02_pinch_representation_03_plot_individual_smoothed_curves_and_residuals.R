setwd("//media//kswada//MyFiles//R//pinch")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pinch
# ------------------------------------------------------------------------------

pinch <- matrix(scan("pinch.txt", 0), 151, 20, byrow=TRUE)

# data("pinch", package = "fda")

head(pinch)



# ------------------------------------------------------------------------------
# plot individual curves and its residuals
# ------------------------------------------------------------------------------

# plot each curve along with the data
plotfit.fd(y = pinch, argvals = pinchtime, fdobj = pinchfd)



# plot the residuals, with cases sorted by size of mean squared residuals
plotfit.fd(pinch, pinchtime, pinchfd, residual=TRUE, sort=TRUE)
