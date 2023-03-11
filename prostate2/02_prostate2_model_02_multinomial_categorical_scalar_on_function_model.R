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
# Multinomial categorical scalar on function model
# ------------------------------------------------------------------------------

# recode for multinom
prostate$type1 <- prostate$type - 1


mod1 <- gam(list(type1 ~ s(MZ, by = intensity, k = 100),
                 ~ s(MZ, by = intensity, k = 100)),
            family = multinom(K = 2), data = prostate)


summary(mod1)



# ------------------------------------------------------------------------------
# Plots:
#   - Estimated coefficient function and confidence band for the ordered categorical prostate diagnosis model
#   - Box plots of the predicted probability of cancer for each subject, by actual status
# ------------------------------------------------------------------------------

pb1 <- predict(mod1, type = "response")

head(pb1)



# ----------
# Estimated coefficient functions for the enlarged class and cancerous class
graphics.off()
par(mfrow = c(1,2))
plot(mod1, rug = FALSE, scheme = 1, xlab = "Daltons", ylab = "f(d)", cex.lab = 1.6, cex.axis = 1.4)



# -->
# Notice that while both are somewhat similar to the single coefficient function in the ordered categorical model,
# there are now somedifferences between them.



# ----------
# top row: box plots of ordered categorical model predicted probabilities for each healthy category by actual category
# bottom row: the same for the multinomial model, which appears to have substantially better discriminatory power.

graphics.off()
par(mfrow = c(2,3))
plot(factor(prostate$type1), pb[,1], xlab = "true status", ylab = "Pr(healthy|1)")
plot(factor(prostate$type1), pb[,2], xlab = "true status", ylab = "Pr(enlarged|2)")
plot(factor(prostate$type1), pb[,3], xlab = "true status", ylab = "Pr(cancerous|3)")

plot(factor(prostate$type1), pb1[,1], xlab = "true status", ylab = "Pr(healthy|1)")
plot(factor(prostate$type1), pb1[,2], xlab = "true status", ylab = "Pr(enlarged|2)")
plot(factor(prostate$type1), pb1[,3], xlab = "true status", ylab = "Pr(cancerous|3)")

