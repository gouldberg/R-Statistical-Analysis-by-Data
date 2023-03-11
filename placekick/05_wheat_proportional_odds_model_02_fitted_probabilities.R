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
# Proportional odds model
# ------------------------------------------------------------------------------

# Order properly the type factor
levels(wheat$type)

wheat$type.order <- factor(wheat$type, levels = c("Scab", "Sprout", "Healthy"))

levels(wheat$type.order)



# ----------
library(MASS)

# The method = "logistic" argument value instructs to use the logit transformation on the cumulative probabilities
mod.fit.ord <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat, method = "logistic")
# mod.fit.ord2 <- polr(formula = type.order ~ class + density + hardness + size + weight + moisture, data = wheat)

summary(mod.fit.ord)
# summary(mod.fit.ord2)



# ------------------------------------------------------------------------------
# Estimated probabilities or classes for each response category
# ------------------------------------------------------------------------------

pi.hat.ord <- predict(object = mod.fit.ord, type = "probs")

head(pi.hat.ord)

head(predict(object = mod.fit.ord, type = "class"))



# ----------
# IMPORTANT !!!!
# The polr() function estimates the model:  logit(P(Y <= j)) = beta - beta1 * x1 - beta2 * x2 ...
# NOT: logit(P(Y <= j)) = beta + beta1 * x1 + beta2 * x2 ...

# -->
# As with model fit objects from multinom(), the predict() function does not provide the estimated variances neede to form Wald confidence intervals.
# In addition, the deltaMethod.polr() method function does not allow for any mathematical functions of the intercept terms, and further problems arise
# because polr() estimates a model as described above.



# ------------------------------------------------------------------------------
# plot estimated probabilities with comparison to multinomial logit model (non-proportional)
# ------------------------------------------------------------------------------
# Estimate model with density only
# non-proportional
mod.fit <- multinom(formula = type ~ density, data = wheat, method = "logistic")
beta.hat <- coef(mod.fit)


# proportional odds model
mod.fit.dens <- polr(formula = type.order ~ density, data = wheat, method = "logistic")
summary(mod.fit.dens)

min(wheat$density)
max(wheat$density)



# ----------
x11(width = 7, height = 6, pointsize = 12)

# Get whole plot area first without model plotted
# pdf(file = "c:\\figures\\Figure3.5color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)), ylab = expression(hat(pi)), xlab = "Density",
      xlim = c(min(wheat$density), max(wheat$density)), col = "black", lty = "solid", lwd = 2, n = 1000, type = "n",
      panel.first = grid(col = "gray", lty = "dotted"))



# ----------
lwd.mult <- 2
# Plot each pi_j for multinommial regression model
curve(expr = 1/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "black", lty = "solid", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Healthy"]), max(wheat$density[wheat$type == "Healthy"])))  # Healthy

curve(expr = exp(beta.hat[1,1] + beta.hat[1,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "green", lty = "dotdash", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Scab"]), max(wheat$density[wheat$type == "Scab"])))  # Scab

curve(expr = exp(beta.hat[2,1] + beta.hat[2,2]*x)/(1 + exp(beta.hat[1,1] + beta.hat[1,2]*x) + exp(beta.hat[2,1] + beta.hat[2,2]*x)),
      col = "red", lty = "longdash", lwd = lwd.mult, n = 1000, add = TRUE,
      xlim = c(min(wheat$density[wheat$type == "Sprout"]), max(wheat$density[wheat$type == "Sprout"])))  # Sprout


# ----------
lwd.po <- 4

# Plot each pi_j for proportional odds model
curve(expr = plogis(q = mod.fit.dens$zeta[1] - mod.fit.dens$coefficients*x), col = "green",
      type = "l", xlim = c(min(wheat$density[wheat$type.order == "Scab"]), max(wheat$density[wheat$type.order == "Scab"])),
      add = TRUE, lty = "dotdash", lwd = lwd.po, n = 1000)  # Scab

curve(expr = plogis(q = mod.fit.dens$zeta[2] - mod.fit.dens$coefficients*x) - plogis(q =mod.fit.dens$zeta[1] - mod.fit.dens$coefficients*x), col = "red",
      type = "l", xlim = c(min(wheat$density[wheat$type.order == "Sprout"]), max(wheat$density[wheat$type.order == "Sprout"])),
      add = TRUE, lty = "longdash", lwd = lwd.po, n = 1000)  # Sprout

curve(expr = 1 - plogis(q = mod.fit.dens$zeta[2] - mod.fit.dens$coefficients*x), col = "black",
      type = "l", xlim = c(min(wheat$density[wheat$type.order == "Healthy"]), max(wheat$density[wheat$type.order == "Healthy"])),
      add = TRUE, lty = "solid", lwd = lwd.po, n = 1000)  # Healthy

legend(x = 1.4, y = 0.8, legend=c("Healthy", "Sprout", "Scab"), lty=c("solid","longdash","dotdash"),
       col=c("black","red","green"), bty="n", lwd = c(2,2,2), seg.len = 4)

# dev.off()


# -->
# Thicker line: estimated proportional odds model
# Thinner line: estimated non-proportional multinomial logit model
# These models produce somewhat similar probability curves with the same scab < sprout > healthy ordering for density with both models.
# This provides some reassurance that the ordering used for the proportional odds model is appropriate and
# that the assumption of proportional odds among cumulative probabilities is reasonable.


# Note that I was not able to get curve(expr = predict(object = mod.fit.ord, newdata = data.frame(density = x), ...) to work



# ------------------------------------------------------------------------------
# Histograms of the observed density levels help to show the density levels with respect to the kernel types
# ------------------------------------------------------------------------------

par(mfrow = c(3,1))

hist(wheat$density[wheat$type.order == "Scab"], xlim = c(0.5, 2.1), main = "Scab", col = "green", 
     breaks = seq(from = 0.5, to = 2.1, by = 0.1), xlab = "Density")

hist(wheat$density[wheat$type.order == "Sprout"], xlim = c(0.5, 2.1), main = "Sprout", col = "red", 
     breaks = seq(from = 0.5, to = 2.1, by = 0.1), xlab = "Density")

hist(wheat$density[wheat$type.order == "Healthy"], xlim = c(0.5, 2.1), main = "Healthy", col = "black", 
     breaks = seq(from = 0.5, to = 2.1, by = 0.1), xlab = "Density")

par(mfrow = c(1,1))



