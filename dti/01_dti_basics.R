setwd("//media//kswada//MyFiles//R//dti")

packages <- c("dplyr", "fda", "refund")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Diffusion tensor imaging (DTI)
#   - Diffusion tensor imaging (DTI) is a magnetic resonance imaging methodology which is used to measure the diffusion of water in the brain.
#     Water diffuses isotropically (i.e. the same in all directions) in the brain except in white matter where it diffuses anisotropically
#     (i.e. differently in different directions).
#     This allows researchers to utilize DTI to generate images of white matter in the brain.
#     Understanding the structure of the brain is important for a wide range of neurological conditions and diseases including Multiple Sclerosis.
#   - Data were collected at Johns Hopkins University and the Kennedy-Krieger Institute.
# ------------------------------------------------------------------------------
data("DTI", package = "refund")

str(DTI)



# ----------
# We consider fractional anisotropy tract profiles of the corpus callosum, a portion of the data DTI data set.
# The corpus collosum, the largest white matter structure in the brain, is a bundle of nerve fibers connecting the two hemispheres of the brain.
# Fractional anisotropy is a value between 0 and 1 which measures the level of anisotropy, and therefore the quantity of white matter,
# at a particular location
# A total of 376 patients are considered, with each tract measured at 93 equally spaced locations.

( Y <- DTI$cca )


# exclude rows with missing values
Y <- Y[-c(126, 130, 131, 125, 319, 321),]



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
