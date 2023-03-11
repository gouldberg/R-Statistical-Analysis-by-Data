setwd("//media//kswada//MyFiles//R//wilpat")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  WilPat
# ------------------------------------------------------------------------------

data("WilPat", package = "MPsychoR")

str(WilPat)



# ----------
wpit15 <- WilPat[,1:15]

names(wpit15)



# ------------------------------------------------------------------------------
# Assess unidimensionality
#   - fitting a 2-dimensional Homals to remaining nominal responses
#   - Homals is a more general model than Princals where the focus is on scoring the categories
# ------------------------------------------------------------------------------

# Eliminate 4 items based on 2-dimensional Princals analysis
elim <- c("Nationalism", "Patriotism", "ChurchAuthority", "Obedience")

ind <- match(elim, colnames(wpit15))

wpitnew <- wpit15[, -ind]



# ----------
library(Gifi)

wpihom <- homals(wpitnew)

summary(wpihom)


# 2D Homals joint category plot
plot(wpihom)



# ----------
# for comparison: Princals
wpiprin2 <- princals(wpitnew, ordinal = FALSE)

wpiprin2


# Nominal Princals loadings plot
plot(wpiprin2)





