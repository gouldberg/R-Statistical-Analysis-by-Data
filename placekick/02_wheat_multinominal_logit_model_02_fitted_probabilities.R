setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)



# ------------------------------------------------------------------------------
# multinominal logit model
# ------------------------------------------------------------------------------

levels(wheat$type)


library(nnet)

methods(class = multinom)

mod.fit <- multinom(formula = type ~ class + density + hardness + size + weight + moisture, data = wheat)


summary(mod.fit)



# ------------------------------------------------------------------------------
# Estimated probabilities and classification
# ------------------------------------------------------------------------------

# Estimate probability of being in a particular category
pi.hat <- predict(object = mod.fit, newdata = wheat, type = "probs")

head(pi.hat)



# ----------
# Predicted classification:  this is helpful for discriminant analysis purposes
predict(object = mod.fit, newdata = wheat, type = "class")



# ----------
# Compute pi.hat using formulas for first record
expl.var <- c(1,0, as.numeric(wheat[1,2:6]))
round(expl.var, 4)

beta.hat <- coefficients(mod.fit)
scab.part <- exp(sum(beta.hat[1,] * expl.var)) 
sprout.part <- exp(sum(beta.hat[2,] * expl.var)) 
pi.hat.scab <- scab.part/(1 + scab.part + sprout.part)
pi.hat.sprout <- sprout.part/(1 + scab.part + sprout.part)
pi.hat.healthy <- 1/(1 + scab.part + sprout.part)

round(data.frame(pi.hat.healthy, pi.hat.scab, pi.hat.sprout), 4)



# ------------------------------------------------------------------------------
# Estimated probabilities where density is the only explanatory variable
# ------------------------------------------------------------------------------

mod.fit.nom.density <- multinom(formula = type ~ density, data = wheat)

summary(mod.fit.nom.density)

beta.hat <- coefficients(mod.fit.nom.density)


par(mfrow=c(2,1), mar=c(2,2,2,2))



# Create plotting area first to make sure get the whole region with respect to x-axis
curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)), ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "black", lty = "solid", lwd = 2, n = 1000, 
      type = "n",
      panel.first = grid(col = "gray", lty = "dotted"))


# ----------
# Plot each pi_j: Healthy
curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "solid", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Healthy"]), max(wheat$density[wheat$type == "Healthy"])))

# Plot each pi_j: Scab
curve(expr = exp(beta.hat[1,1] + beta.hat[1,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "green", lty = "dotdash", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Scab"]), max(wheat$density[wheat$type == "Scab"])))

# Plot each pi_j: Sprout
curve(expr = exp(beta.hat[2,1] + beta.hat[2,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "red", lty = "longdash", lwd = 2, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Sprout"]), max(wheat$density[wheat$type == "Sprout"])))


legend(x = 1.4, y = 0.8, legend=c("Healthy", "Sprout", "Scab"), lty=c("solid","longdash","dotdash"),
       col=c("black","red","green"), bty="n", lwd = c(2,2,2), seg.len = 4)



# -->
# We see that the estimated scab probability is the largest for the smaller density kernels.
# The estimated healthy probability is the largest for the high density kernels.



# ------------------------------------------------------------------------------
# Estimated probabilities where variables other than density are set by mean values
# ------------------------------------------------------------------------------
curve(expr = predict(object = mod.fit, 
      newdata = data.frame(
        class = "hrw", density = x,
        hardness = mean(wheat$hardness), size = mean(wheat$size), weight = mean(wheat$weight),
        moisture = mean(wheat$moisture)),
      type = "probs")[,1], ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "black", lty = "solid", lwd = 2, n = 1000,
      panel.first = grid(col = "gray", lty = "dotted"))


curve(expr = predict(object = mod.fit,
      newdata = data.frame(
        class = "hrw", density = x,
        hardness = mean(wheat$hardness), size = mean(wheat$size), weight = mean(wheat$weight),
        moisture = mean(wheat$moisture)),
      type = "probs")[,2], ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "green", lty = "dotdash", lwd = 2, n = 1000,
      add = TRUE, panel.first = grid(col = "gray", lty = "dotted"))


curve(expr = predict(object = mod.fit,
      newdata = data.frame(
        class = "hrw", density = x,
        hardness = mean(wheat$hardness), size = mean(wheat$size), weight = mean(wheat$weight),
        moisture = mean(wheat$moisture)),
      type = "probs")[,3], ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "red", lty = "longdash", lwd = 2, n = 1000,
      add = TRUE, panel.first = grid(col = "gray", lty = "dotted"))


legend(x = 1.4, y = 0.8, legend=c("Healthy", "Sprout", "Scab"), lty=c("solid","longdash","dotdash"),
       col=c("black","red","green"), bty="n", lwd = c(2,2,2), seg.len = 4)



# ----------
library(effects)

plot(predictorEffects(mod.fit))




