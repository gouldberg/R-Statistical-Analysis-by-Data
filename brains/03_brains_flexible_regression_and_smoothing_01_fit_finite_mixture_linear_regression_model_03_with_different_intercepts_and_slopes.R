setwd("//media//kswada//MyFiles//R//brains")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brains
# ------------------------------------------------------------------------------

data(brains, package = "gamlss.mx")

str(brains)


car::some(brains)



# ----------
# Since the distribution of both brain size and body weight are highly skewed, a log transformation was applied to both
# variables to give transformed variables

brains <- transform(brains, lbrain = log(brain), lbody= log(body))



# ------------------------------------------------------------------------------
# Fit other alternative model
# ------------------------------------------------------------------------------

# If "MASS" is included in the predictor for a distribution parameter (any of sigma, nu, or tau), then the predictor intercepts differ between the K components.

# mu intercept: different  +  mu slope: same  + sigma: different
br.31 <- gamlssNP(formula = lbrain ~ lbody, sigma.fo = ~ MASS, mixture = "np", K = 3, tol = 1, data = brains, family = NO)


# mu intercept: different  +  mu slope: different  + sigma: same
br.32 <- gamlssNP(formula = lbrain ~ lbody, random = ~ lbody, sigma.fo = ~1, mixture = "np", K = 3, tol = 1, data = brains, family = NO)


# mu intercept: different  +  mu slope: different  + sigma: different
br.33 <- gamlssNP(formula = lbrain ~ lbody, random = ~ lbody, sigma.fo = ~MASS, mixture = "np", K = 3, tol = 1, data = brains, family = NO)



# ----------
GAIC(br.3, br.31, br.32, br.33)

GAIC(br.3, br.31, br.32, br.33, k = log(length(brains$lbody)))



# -->
# Model br.3 has the smallest SBC value.
# (Note that model br.32 has the smallest AIC value, however with so many parameters in the model and so few data points the model
# produces rather nonsensical results.)

# Note also that, in general, since model br.33 has components with no parameters in common it could also be fitted
# using gamlssMX().
# Again with only 27 observations any fitted model is too sensitive to starting values.




# ------------------------------------------------------------------------------
# Plot fitted values and weighted average for br.33 model
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))
with(brains, plot(lbody, lbrain, 
                  pch = c(21, 22, 23)[max.col(br.3$post.prob[[1]])],
                  bg = c("red", "green3", "blue")[max.col(br.3$post.prob[[1]])]))

for(k in 0:3){
  with(brains, lines(fitted(br.33, K = k)[order(lbody)] ~ lbody[order(lbody)],
                     lty = k+1, lwd = 2, col = c("black", "red", "green3", "blue")[k+1]))
}

legend("topleft", legend = c("Weighted Average", "Component 1", "Component 2", "Component 3"), pch = c(20, 21, 22, 23),
       pt.bg = c("black", "red", "green3", "blue"), lty = 1:4, lwd = 2, col = c("black", "red", "green3", "blue"))



# ----------
# The weighted average for the (conditional) parameters mu for the K components for each observation
fitted(br.33, K = 0)

