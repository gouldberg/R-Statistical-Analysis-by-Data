setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ------------------------------------------------------------------------------
# Beta Regression
#   - The advantage of the Beta-based model is the full distributional model which would allow the construction of full predictive distributions
#     rather than just a point estimate and standard error.
# ------------------------------------------------------------------------------

library(mgcv)


# The default choice of link is the logit funciton.

# modb <- gam(pdr ~ log(body) + log(lifespan) + danger, family = betar(), mammalsleep)
modb <- gam(pdr ~ log(body) + log(lifespan), family = betar(), mammalsleep)


summary(modb)


summary(qlmod)



# ----------
termplot(modb, partial.resid = TRUE, rug = TRUE, ask = FALSE)



# ----------
residualPlot(modb)

residualPlot(modb, "log(body)")


# anova(qlmod, modb, test = "Chisq")
