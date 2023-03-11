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
# Determining the number of factors:  Tucker-Lewis index (TLI)
#   - it compares a worst-case model Q0 (a zero-factor model) and a best-case model with our fitted model Qp
#     TLI(p) = ( Q0 - Qp ) / ( Q0 - 1 )
#   - the larger its value the better the fit.  Hu and Bentler (1999) consider values above 0.90 as "good" fit
# ------------------------------------------------------------------------------

fa_1 <- fa(itceaq, 1, cor = "poly", fm = "ml")

fa_2 <- fa(itceaq, 2, cor = "poly", fm = "ml")

fa_3 <- fa(itceaq, 3, cor = "poly", fm = "ml")



# ----------
summary(fa_1)


summary(fa_2)


summary(fa_3)



# -->
# TLI(1) = 0.696   TLI(2) = 0.759   TLI(3) = 0.804 < 0.90
# We see that the TLI is smaller than 0.90, and the RMSEA indicates NOT good fit (= 0.098 > 0.05)
