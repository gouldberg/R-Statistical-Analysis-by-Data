setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# Tests for random effects, spatial and serial error correlation
# Spatial and serial correlation
# ------------------------------------------------------------------------------


# The result from the C.2 test, although not very sharp, 
# warrants an investigation into the serial correlation issue.
# To this end, we estiamte the full SEMSRRE model, visualizing only the significance table for the error components.
# t-statistics are expected to mimic the results of the asymptotically equivalent LM tests closely:
# Still there is more information to be extracted from the encompassing model, i.e., the magnitudes, and hence the substantial importance,
# of the estimated parameters.

semsrre.rice <- spreml(riceprod, data = Rice, w = riceww, lag = FALSE, errors = "semsrre")


round(summary(semsrre.rice)$ErrCompTable, 6)




# -->
# Spatial error correlation is confirmed as the statistically strongest effect, with the now familiar large coefficient.
# Both individual effects and serial correlation play minor roles:
# the variance ratio of the random effects over the idiosyncratic errors phi is about one fourth;
# the estimated serial correlation coefficients is but 0.13.
