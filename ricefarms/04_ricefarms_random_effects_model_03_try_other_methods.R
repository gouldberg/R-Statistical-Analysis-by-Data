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
# Compare the results obtained with the 4 estiamtion methods
# ------------------------------------------------------------------------------

r.walhus <- update(r.swar, random.method = "swar")


r.amemiya <- update(r.swar, random.method = "amemiya")


r.nerlove <- update(r.swar, random.method = "nerlove")


r.models <- list(swar = r.swar, walhus = r.walhus, amemiya = r.amemiya, nerlove = r.nerlove)



# ----------
sapply(r.models, function(x) ercomp(x)$theta)


sapply(r.models, coef)



# -->
# These are very close to each other, and consequently, the estimated coefficients for the 4 models are almost identical

# theta by nerlove is quite different from others...
