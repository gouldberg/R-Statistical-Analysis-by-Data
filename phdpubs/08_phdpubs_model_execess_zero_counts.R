setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ------------------------------------------------------------------------------
# Zero-inflated Model
#   - This model postulates that the observed counts arise from a mixture of two latent classes of observations
#     Some structural zeros for whom y will always be 0, and the rest, sometimes giving random zeros
# ------------------------------------------------------------------------------

library(pscl)

mod.zpois <- zeroinfl(articles ~ . | mentor, data = PhdPubs, dist = "poisson")

mod.znbin <- zeroinfl(articles ~ . | mentor, data = PhdPubs, dist = "negbin")



# ----------
summary(mod.zpois)


summary(mod.znbin)



# ----------
plot_resid(mod=mod.zpois, y = "articles")


plot_resid(mod=mod.znbin, y = "articles")




# ------------------------------------------------------------------------------
# Hurdle Model (zero-altered model)
#   - This model also uses a separate logistic regression submodel to distinguish counts of y = 0 from larger counts, y > 0
#     The submodel for the positive counts is expressed as a (left) truncated Poisson or negativ-binomial model, excluding the zero counts.
# ------------------------------------------------------------------------------


mod.hpois <- hurdle(articles ~ ., data = PhdPubs, dist = "poisson")


mod.hnbin <- hurdle(articles ~ ., data = PhdPubs, dist = "negbin")



# ----------
summary(mod.hpois)


summary(mod.hnbin)



# ----------
plot_resid(mod=mod.hpois, y = "articles")


plot_resid(mod=mod.hnbin, y = "articles")

# plot_resid(mod=mod.nbin, y = "articles")

