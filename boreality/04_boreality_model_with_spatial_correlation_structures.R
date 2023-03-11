# setwd("//media//kswada//MyFiles//R//boreality")
setwd("//media//kswada//MyFiles//R//Generalized_linear_model_and_heterogeneity//boreality")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  boreality
# ------------------------------------------------------------------------------

Boreality <- read.table(file = "Boreality.txt", header = TRUE)


str(Boreality)


dim(Boreality)


car::some(Boreality)



# ----------
# Boreality was transformed using the following transformation (See Cressie (p.395, 1993))
# nBor:  the number of species that belong to breal coenosis species
# nTot:  number of all species at the site
Boreality$Bor <- sqrt(1000 * (Boreality$nBor + 1) / (Boreality$nTot))



# ------------------------------------------------------------------------------
# Add spatial correlation structures
#   - Exponential correlation:  corExp
#   - Gaussian correlation:  corGaus
#   - Linear correlation:  corLin
#   - Rational quadratic correlation:  corRatio
#   - Spherical correlation:  corSpher
# ------------------------------------------------------------------------------

library(nlme)


f1 <- formula(Bor ~ Wet)


B1A <- gls(f1, correlation = corSpher(form = ~ x + y, nugget = TRUE), data = Boreality)


# corLin does not converge
B1B <- gls(f1, correlation = corLin(form = ~ x + y, nugget = TRUE), data = Boreality)


B1C <- gls(f1, correlation = corRatio(form = ~ x + y, nugget = TRUE), data = Boreality)


B1D <- gls(f1, correlation = corGaus(form = ~ x + y, nugget = TRUE), data = Boreality)


B1E <- gls(f1, correlation = corExp(form = ~ x + y, nugget = TRUE), data = Boreality)



# ----------
AIC(B1.gls, B1A, B1C, B1D, B1E)



# -->
# AIC of the model with no correlation is 2844.541, but the model with spatial correlation have considerable lower AIC values
# B1E (corExp structure) is the best in terms of AIC



# ----------
summary(B1E)


# -->
# range (the point along distance at which this pattern levels off):  481.172
# nugget (semivariogram when the distance is zero):  0.4849



