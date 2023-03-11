setwd("//media//kswada//MyFiles//R//wing")

packages <- c("dplyr", "pixmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  wing
# ------------------------------------------------------------------------------

M <- read.pnm("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//wing.ppm")

M


str(M)


# -----------
plot(M)




# ------------------------------------------------------------------------------
# Reconstruct of outlines by Fourier analysis applied to equally spaced radii
# ------------------------------------------------------------------------------

# use "ju", the output from "regularradius" in previous analysis

f1 <- fourier1(ju$coord, 32)


par(mar = c(2,2,2,2))
layout(matrix((1:9), 3, 3))

for(i in 1:9){
  if1 <- ifourier1(f1$ao, f1$an, f1$bn, 64, i)
  
  plot(if1$X, if1$Y, asp = 2, type = "l", frame = F, main = paste("Harmonics 0 to", i))
}



# -->
# We notice that the wing outline reconstruction is nearly correct at the seventh harmonic.
# Becauseo of the construction in the middle of the wing, the Fourier decomposition generates harmonics of a higher order than the order
# necessary to reliably describe the wing.
# One can decide to drop high order harmonics and to keep the first ones only, if the middle region of the wing is not considered as important for the study.

# Note that the starting point does not correspond to the one that we have first digitized.


# -->
# There can be analytical problems with equally spaced radii Fourier analysis:
# Indeed, the data itself can be flawed since the sampling strategy gathers more information in some parts of the outline and much less elsewhere.
# In addition, one cannot apply the method when a given radius twice intercepts the outline
# (it can arise when the outline presents pronounced convexities or concativities)


