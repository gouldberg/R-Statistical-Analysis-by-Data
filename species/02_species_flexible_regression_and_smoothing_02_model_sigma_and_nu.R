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
# Model log(sigma)
# ------------------------------------------------------------------------------

# excluding the Poissson distribution which does not have a sigma parameter
fam <- c("DPO", "NBI", "NBII", "PIG", "DEL", "SICHEL")


m.ql <- list()

for(i in 1:length(fam)){
  
  m.ql[[fam[i]]] <- GAIC(gamlss(fish ~ poly(x, 2), sigma.fo = ~x, data = species, family = fam[i], n.cyc = 60, trace = FALSE), k = 2)
}



# ----------
unlist(m.ql)



# -->
# Modelling log(sigma) as a linear function of x improves the AIC for all models.
# The PIG model is still the best.



# ------------------------------------------------------------------------------
# Model nu
# ------------------------------------------------------------------------------

# the Sichel and the Delaporte distributions have three parameters  (third one is nu)
# Sichel uses the identity as the default link for nu while the Delaprte uses the logit
fam <- c("DEL", "SICHEL")

show.link("DEL")
show.link("SICHEL")


m.qll <- list()

for(i in 1:length(fam)){
  
  m.qll[[fam[i]]] <- GAIC(gamlss(fish ~ poly(x, 2), sigma.fo = ~x, nu.fo = ~x, data = species, family = fam[i], n.cyc = 60, trace = FALSE), k = 2)
}



# ----------
unlist(m.qll)



# -->
# Modelling the predictor fof nu as a linear function of x improves the Sichel model,
# which now has a lower AIC than the PIG model, but not the Delaporte model.



# ------------------------------------------------------------------------------
# Further simplification of SICHEL model
# ------------------------------------------------------------------------------

# Further simplification of the Sichel model can be achieved by dropping the linear term in x for the lo(sigma) model wichi does not contribute anything to the fit.

mSI <- gamlss(fish ~ poly(x, 2), data = species, sigma.fo = ~1, nu.fo = ~x, family = SICHEL, n.cyc = 60, trace = FALSE)


GAIC(mSI)


fittedPlot(mSI, x = species$x)




# ------------------------------------------------------------------------------
# Final model and fitted distributions
# ------------------------------------------------------------------------------

# plot the final model's fitted values

plot(fish ~ log(lake), data = species)

lines(species$x[order(species$lake)], fitted(mSI)[order(species$lake)], col = "red")



# ----------
# fitted distributions for observation 7 and 68

pdf.plot(mSI, c(7, 68), min = 0, max = 120, step = 1)


# -->
# for observation 7:  lake area of 44 km2, x = log(44) = 3.74, and mu = 19.37, sigma = 1.44, nu = -7.18)
# for observation 68:  lake area of 9,065 km2, x = log(9065) = 9.11, and mu = 48.86, sigma = 1.44, nu = -1.10)

# Note that the vertical scale is different for 2 plots.


