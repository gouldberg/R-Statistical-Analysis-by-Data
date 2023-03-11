setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
# ------------------------------------------------------------------------------

data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)



# ----------
# set the lowest category to zero in order to make it eRm compatible
itceaq <- CEAQ[,1:16] - 1



# ----------
library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho




# ------------------------------------------------------------------------------
# try factor analysis with removing outlier items
# ------------------------------------------------------------------------------

# removing ceaq10, 14, 15

itceaq2 <- itceaq[,-c(10,14,15)]


polcor2 <- polychoric(itceaq2)



# ----------
resFA_s <- fa(polcor2$rho, nfactors = 2, rotate = "none", fm = "ml")


summary(resFA_s)


print(resFA_s$loadings, cutoff = 0.2)


round(resFA_s$communality, 2)



# ----------
resFA_s2 <- fa(polcor2$rho, nfactors = 2, rotate = "varimax", fm = "ml")

print(resFA_s2$loadings, cutoff = 0.2)




# ----------
resFA_s3 <- fa(polcor2$rho, nfactors = 2, rotate = "oblimin", fm = "ml")

resFA_s4 <- fa(polcor2$rho, nfactors = 3, rotate = "oblimin", fm = "ml")


print(resFA_s3$loadings, cutoff = 0.2)

print(resFA_s4$loadings, cutoff = 0.2)

print(resFA4$loadings, cutoff = 0.2)



