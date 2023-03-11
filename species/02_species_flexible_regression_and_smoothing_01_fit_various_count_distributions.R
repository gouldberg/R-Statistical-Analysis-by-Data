setwd("//media//kswada//MyFiles//R//species")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  species
# ------------------------------------------------------------------------------
data("species", package = "gamlss.data")


str(species)

car::some(species)



# ----------
species <- transform(species, x = log(lake))



# ------------------------------------------------------------------------------
# Fit 7 different count distributions
# ------------------------------------------------------------------------------

# the count distributions:
# Poisson (PO), double Poisson (DPO), negative binomial types I and II (NBI, NBII), Poisson inverse Gaussian (PIG), Delaprte (DEL) and Sichel (SICHEL)
fam <- c("PO", "DPO", "NBI", "NBII", "PIG", "DEL", "SICHEL")



# fitting the linear in x models and quadratic in x models
m.l <- m.q <- list()

for(i in 1:length(fam)){
  
  m.l[[fam[i]]] <- GAIC(gamlss(fish ~ x, data = species, family = fam[i], n.cyc = 60, trace = FALSE), k = 2)

  m.q[[fam[i]]] <- GAIC(gamlss(fish ~ poly(x, 2), data = species, family = fam[i], n.cyc = 60, trace = FALSE), k = 2)
}



# ----------
unlist(m.l)

unlist(m.q)



# -->
# Poisson model has a very large AIC compared to the rest of the distributions, so we can conclude that the data are overdispersed

# The quadratic polynomial in x seems to fit better than the linear term across the different count distributions (except for DPO),
# as judged by AIC.

# The best model at this stage is the POissson inverse Gaussian (PIG) model with a quadratic polynomial in x.



# ------------------------------------------------------------------------------
# Fit P-spline smoothing
# ------------------------------------------------------------------------------

GAIC(m.pb <- gamlss(fish ~ pb(x), data = species, family = PIG, trace = FALSE))


m.pb$mu.df



# ----------
# The P-spline smoothing does not seem to improve the model, so we keep the quadratic polynomial in x.







