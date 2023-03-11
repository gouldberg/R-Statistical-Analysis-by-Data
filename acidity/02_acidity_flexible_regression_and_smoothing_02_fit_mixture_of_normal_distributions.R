setwd("//media//kswada//MyFiles//R//acidity")

packages <- c("dplyr", "gamlss", "gamlss.mx")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  acidity
# ------------------------------------------------------------------------------

data(acidity, package = "gamlss.data")

str(acidity)


psych::describe(acidity)



# ------------------------------------------------------------------------------
# Fit a mixture of two normal distributions
# ------------------------------------------------------------------------------

# n: the number of fits required in gamlssMXfits()
mm <- gamlssMXfits(n = 10, y ~ 1, family = NO, K = 2, data = acidity)

mm



# ----------
# Calculate the pdf for the fitted mixture

afn <- getpdfMX(mm)

afn



# ----------
h1 <- histSmo(acidity$y, plot = TRUE)



# ----------
with(acidity, truehist(y))

lines(seq(1, 8, .01), afn(seq(1, 8, .01)), col = "red")

lines(h1, col = "blue")

