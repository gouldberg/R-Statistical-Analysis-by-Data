setwd("//media//kswada//MyFiles//R//prostate2")

packages <- c("dplyr", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prostate
# ------------------------------------------------------------------------------

data(prostate, package = "gamair")


str(prostate)


car::some(prostate)


prostate$intensity



# ------------------------------------------------------------------------------
# Ordered categorical scalar on function model
# ------------------------------------------------------------------------------

# ocat specifies the ordered categorical model with R = 3 classes.
# The estimated smooth function is quite complex, but also quite robust to the choice of basis and basis dimension.

mod <- gam(type ~ s(MZ, by = intensity, k = 100), family = ocat(R = 3), data = prostate, method = "ML")


summary(mod)



# ------------------------------------------------------------------------------
# Plots:
#   - Estimated coefficient function and confidence band for the ordered categorical prostate diagnosis model
#   - Box plots of the predicted probability of cancer for each subject, by actual status
#   - Sorted deviance residuals for the model against simulated theoretical quantiles, with reference band in grey.
# ------------------------------------------------------------------------------

pb <- predict(mod, type = "response")


graphics.off()
par(mfrow = c(1,3))
plot(mod, rug = FALSE, scheme = 1, xlab = "Daltons", ylab = "f(d)", cex.lab = 1.6, cex.axis = 1.4)
plot(factor(prostate$type), pb[,3])
qq.gam(mod, rep = 100, lev = 0.95)



# -->
# Boxplots of predicted probability is somewhat disappointing.
# The model does not distinguish cancer from non-cancer very sharply, and the problem-specific algorithm described in Adam et al. (2002) does better in this case.
