setwd("//media//kswada//MyFiles//R//wesdr")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  wesdr
# ------------------------------------------------------------------------------
data(wesdr, package = "gamair")

str(wesdr)



# ------------------------------------------------------------------------------
# Smooth ANOVA with intercation and model selection by null space penalties
# ------------------------------------------------------------------------------

# include smooth "interactions" constructed to exclude the main effects by ti()
# For model selection, the null space penalties can be used.

k <- 7
# k <- 6

b <- gam(ret ~ s(dur, k = k) + s(gly, k = k) + s(bmi, k = k) + ti(dur, gly, k = k) + ti(dur, bmi, k = k) + ti(gly, bmi, k = k), 
         select = TRUE, data = wesdr, family = binomial(), method = "ML")

b

summary(b)


graphics.off()
plot(b)


# -->
# Notice how ti(fur, gly, k = k) and ti(dur, bmi, k = k) are effectively penalized out of the model
# ti(gly, bmi, k = k) is estimated to be non-zero and significant.



# ----------
vis.gam(b, color = "bw", view = c("dur", "gly"), plot.type = "persp", theta = 300, phi = 30)

vis.gam(b, color = "bw", view = c("dur", "bmi"), plot.type = "persp", theta = 300, phi = 30)

vis.gam(b, color = "bw", view = c("gly", "bmi"), plot.type = "persp", theta = 300, phi = 30)


# We need to interpret the main effects and interaction for bmi and gly together.
# One possibility would be to re-estimate the model with the simplifed model



# ------------------------------------------------------------------------------
# Re-estimate the model with the simplified model
# ------------------------------------------------------------------------------

b2 <- gam(ret ~ s(dur, k = k) + te(gly, bmi, k = k), data = wesdr, family = binomial(), method = "ML")

b2

summary(b)
summary(b2)


AIC(b, b2)


# -->
# This model leads to small increase in ML score and AIC
# since the model is changed slighly by the change in smoothing penalties between the smooth ANOVA model and the simpler model.


# ----------
# Estimated duration effect
plot(b2)



# ----------
# Combined effect of body mass index and percent glycosylated hemoglobin
# The grey surfaces are at plus or minus one standard error from the estimate
graphics.off()
vis.gam(b2, se = 1, color = "bw", view = c("gly", "bmi"), plot.type = "persp", theta = 300, phi = 30)


# -->
# Notice how a higher level of glycosylated hemoglobin seems to be torerated when body mass index is low



# --> The summary:
# It seems that risk increase with increasing glycosylated hemoglobin, as expected,
# but initially increase less steeply for those with low BMI.
# Similarly, risk increase initially with disease duration, but there is then some evidence of decline.
# It could be that the patients only get to have high duration if they tend to have their disease under good control, or it could be that 
# there are just some patients who were not going to get reinopathy, while those less fortunate tended to get it earlier rather than later
# in their disease progression.



# ----------
gam.check(b2)


# -->
# Note that not much checking has been done for this example
# since model checking with binary response data is difficult,
# but the flexibility of the assumed model does offer some defence against misspecification problems.

