setwd("//media//kswada//MyFiles//R//wcgs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wcgs
# ------------------------------------------------------------------------------

data("wcgs", package = "faraway")

str(wcgs)



# ------------------------------------------------------------------------------
# Goodness of Fit:  Observed Proportion vs. Binned predicted probabilities
# ------------------------------------------------------------------------------

wcgsm <- na.omit(wcgs)

wcgsm <- mutate(wcgsm, predprob = predict(lmod, type = "response"))

gdf <- group_by(wcgsm, cut(linpred, breaks = unique(quantile(linpred, (1:100)/101))))

hldf <- summarise(gdf, y = sum(y), ppred = mean(predprob), count=n())

hldf <- mutate(hldf, se.fit = sqrt(ppred * (1 - ppred) / count))


ggplot(hldf, aes(x = ppred, y = y / count, ymin = y / count - 2 * se.fit, ymax = y / count + 2 * se.fit)) +
  geom_point() + geom_abline(intercept = 0, slope = 1) +
  xlab("Predicted Probability") + ylab("Observed Proportion") +
  geom_linerange(color = grey(0.75))


# -->
# For a well-calibrated prediction model, the observed proportions and predicted probablities should be close.
# Although we can see there is some variation, there is no consistent deviation from what is expected.
# We have computed approximate 95% confidence intervals using the binomial variation.
# The line passes through most of these intervals confirming that the variation from the expected is no excessive.



# ------------------------------------------------------------------------------
# Goodness of Fit:  Hosmer-Lemeshow statistic
# ------------------------------------------------------------------------------

# The Hosmer-Lemeshow statistic formalizes this assessment.
# We need sufficient observations per bin to ensure the accuracy of the X^2 approximation.

hlstat <- with(hldf, sum( (y - count * ppred)^2 / (count * ppred * (1 - ppred))))

c(hlstat, nrow(hldf))

1 - pchisq(hlstat, nrow(hldf) - 1)


# -->
# Since the p-value is moderate, we detect no lack of fit.
# In marginal cases, it is worth experimenting with differing numbers of bins to check the robustness of the conclusion.



# ------------------------------------------------------------------------------
# Goodness of Fit:  Scoring method (ROC Curve)
# ------------------------------------------------------------------------------

wcgsm <- mutate(wcgsm, predout = ifelse(predprob < 0.5, "no", "yes"))

xtabs(~ chd + predout, wcgsm)



# ----------
thresh <- seq(0.01, 0.5, 0.01)
Sensitivity <- numeric(length(thresh))
Specificity <- numeric(length(thresh))


for(j in seq(along=thresh)){
  pp <- ifelse(wcgsm$predprob < thresh[j], "no", "yes")
  xx <- xtabs(~ chd + pp, wcgsm)
  Specificity[j] <- xx[1,1] / (xx[1,1] + xx[1,2])
  Sensitivity[j] <- xx[2,2] / (xx[2,1] + xx[2,2])
}


matplot(thresh, cbind(Sensitivity, Specificity), type = "l", xlab = "Threshld", ylab = "Proportion", lty = 1:2)


plot(1 - Specificity, Sensitivity, type = "l")
abline(0, 1, lty = 2)



# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained  (Nagelkerke)
# ------------------------------------------------------------------------------

lmodr <- glm(chd ~ age + height + bmi + sdp + chol + dibep + cigs + arcus, family = binomial, wcgs)

summary(lmodr)


# ----------
# Nagelkerke R^2
n <- lmodr$df.null + 1

(1 - exp((lmodr$dev - lmodr$null)/n)) / (1 - exp(-lmodr$null/n))



# ----------
# McFadden's peudo R^2
1 - lmodr$deviance / lmodr$null.deviance


# -->
# This gives the impression of a fairly poor, but this is misleading.
# It is quite common to see low values of Naglekerke's and other R^2 substitues even when the model is good.



