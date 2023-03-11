setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ------------------------------------------------------------------------------
# Zero-inflated Model
#   - This model postulates that the observed counts arise from a mixture of two latent classes of observations
#     Some structural zeros for whom y will always be 0, and the rest, sometimes giving random zeros
# ------------------------------------------------------------------------------

library(pscl)

# mod.zpois <- zeroinfl(intensity ~ length + area * year | area * year, data = CodParasites, dist = "poisson")
mod.zpois <- zeroinfl(intensity ~ length + area * year, data = CodParasites, dist = "poisson")

# mod.znbin <- zeroinfl(intensity ~ length + area * year | area * year, data = CodParasites, dist = "negbin")
mod.znbin <- zeroinfl(intensity ~ length + area * year, data = CodParasites, dist = "negbin")



# ----------
summary(mod.zpois)


summary(mod.znbin)



# ----------
plot_resid(mod=mod.zpois, y = "intensity")


plot_resid(mod=mod.znbin, y = "intensity")




# ------------------------------------------------------------------------------
# Hurdle Model (zero-altered model)
#   - This model also uses a separate logistic regression submodel to distinguish counts of y = 0 from larger counts, y > 0
#     The submodel for the positive counts is expressed as a (left) truncated Poisson or negativ-binomial model, excluding the zero counts.
# ------------------------------------------------------------------------------


mod.hpois <- hurdle(intensity ~ length + area * year, data = CodParasites, dist = "poisson")


mod.hnbin <- hurdle(intensity ~ length + area * year, data = CodParasites, dist = "negbin")



# ----------
summary(mod.hpois)


summary(mod.hnbin)



# ----------
plot_resid(mod=mod.hpois, y = "intensity")


plot_resid(mod=mod.hnbin, y = "intensity")

# plot_resid(mod=mod.nbin, y = "intensity")

