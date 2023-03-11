setwd("//media//kswada//MyFiles//R//zareki")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# We consider eight binary subtraction items only
zarsub <- zareki[, grep("subtr", colnames(zareki))]

str(zarsub)



# ----------
# compute tetrachoric correlation matrix

library(psych)

tetcor <- tetrachoric(zarsub)

tetcor

tetcor$rho



# ------------------------------------------------------------------------------
# Determining the number of factors:  Tucker-Lewis index (TLI)
#   - it compares a worst-case model Q0 (a zero-factor model) and a best-case model with our fitted model Qp
#     TLI(p) = ( Q0 - Qp ) / ( Q0 - 1 )
#   - the larger its value the better the fit.  Hu and Bentler (1999) consider values above 0.90 as "good" fit
# ------------------------------------------------------------------------------
fa_1 <- fa(zarsub, 1, cor = "tet", fm = "ml")
fa_2 <- fa(zarsub, 2, cor = "tet", fm = "ml")
fa_3 <- fa(zarsub, 3, cor = "tet", fm = "ml")



# ----------
summary(fa_1)

summary(fa_2)

summary(fa_3)


# -->
# TLI(1) = 0.813   TLI(2) = 0.873   TLI(3) = 0.973 > 0.90
# We see that the TLI is larger than 0.90 with 3 factors, and the RMSEA indicates good fit (= 0.047 < 0.05)
