setwd("//media//kswada//MyFiles//R//hodges")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hodges
# ------------------------------------------------------------------------------

data("hodges", package = "gamlss.data")


str(hodges)

car::some(hodges)



# ------------------------------------------------------------------------------
# Extract estimates for the variance components:  sigma_b
# ------------------------------------------------------------------------------

# Note that getting sigma_b from the lme() output i rather awkward, so we have created a small function to do that.
getSigmab <- function(obj){
  
  vc <- VarCorr(obj)
  
  suppressWarnings(storage.mode(vc) <- "numeric")
  
  vc[1:2, "StdDev"][1]
}


names(getSmo(m3))


getSmo(m2)$sigb


# ML
getSigmab(getSmo(m3))
getSigmab(l2)


# REML
getSigmab(getSmo(m31))
getSigmab(l1)




# ------------------------------------------------------------------------------
# Extract estimates for the variance components:  fitted sigma_e within the gamlss algorithm
# ------------------------------------------------------------------------------

getSmo(m2)$sige

getSmo(m3)$sigma

getSmo(m31)$sigma



# ------------------------------------------------------------------------------
# Extract estimates for the variance components:  sigma_e
# ------------------------------------------------------------------------------

# adjusted estimate of sigma_e
# This is a consequence of the MAP estimation of random effects within GAMLSS
fitted(m2, "sigma")[1]
fitted(m2, "sigma")[1] * getSmo(m2)$sige
l2$sigma

fitted(m3, "sigma")[1]
fitted(m3, "sigma")[1] * getSmo(m3)$sigma



