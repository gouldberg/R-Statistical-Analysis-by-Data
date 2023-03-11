setwd("//media//kswada//MyFiles//R//arthritis")

packages <- c("dplyr", "vcd", "MASS", "datasets", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arthritis
# ------------------------------------------------------------------------------
data("Arthritis", package = "vcd")

data <- Arthritis

data



# ------------------------------------------------------------------------------
# Check zero frequency cells
#  - potential impact to logistic regression's predictive performance
# ------------------------------------------------------------------------------
( dat.tab <- xtabs(~ Treatment + Improved + Sex, data = data) )


ftable(dat.tab)
ftable(dat.tab, row.vars = c("Sex", "Treatment"))



# ------------------------------------------------------------------------------
# Plotting conditional distributions of f(Age | Better) as a histtogram, boxplot, or density plot
# ------------------------------------------------------------------------------
with(data, 
     popbio::logi.hist.plot(Age, Improved > "None", 
                            type = "hist", counts = TRUE, ylabel = "Probability (Better)", xlab = "Age", col.hist = "lightblue"))



# ------------------------------------------------------------------------------
# Fitting a logistic regression model
# ------------------------------------------------------------------------------
# The response variable "Improved" has three categories (none, some, or marked improvement), but for now we consider whether the patient showed
# any improvement at all, defining the event Better to be some or marked improvement.
data$Better <- as.numeric(data$Improved > "None")

arth.logistic <- glm(Better ~ Age, data = data, family = binomial)



# ----------
# lmtest::coeftest() is simple version of summary()
summary(arth.logistic)
lmtest::coeftest(arth.logistic)



# ----------
# estimated odds of a better response (=1.05) for each one-year increase in age
exp(coef(arth.logistic)["Age"])


# Over 10 years, the odds are multiplied by 1.64, 64% increase, a substantial effect in the range for these data.
exp(10 * coef(arth.logistic)["Age"])



# ----------
# Fitted logistic regression curve has slope equal to beta * prob * (1 - prob).
# This has maximum value of beta/4 when prob = 0.5.
# As a rule of thumb, beta/4 gives a quick estimate of the maximum effect of x on the probability scale.
# one-year increase gives 1.2% increase of probability of "Better" at maximum
coef(arth.logistic)["Age"] / 4



# ------------------------------------------------------------------------------
# for comparison, linear probability model
# ------------------------------------------------------------------------------
arth.lm <- glm(Better ~ Age, data = data)
coef(arth.lm)


# The coefficient for age can be interpreted to indicate that the probability of a better response increases by 0.011
# for each one-year increase in age.



# ------------------------------------------------------------------------------
# Model tests for simple logistic regression
# ------------------------------------------------------------------------------
# Wald test of the coefficient for age, testing hypothesis H0: beta = 0 
summary(arth.logistic)



# ----------
# direct test compares the deviance of the fitted model to the deviance of the null model (Chisq test)
# the deviance:  -2 times the log-likelihood ratio of some reduced model to the full model
anova(arth.logistic, test = "Chisq")



# ----------
# How bad is this model, compared to a saturated model ? (a test of the size of the residual deviance)
vcdExtra::LRstats(arth.logistic)


# --> lnear logistic model fits significantly better than the null model, but the model also shows significant lack of fit



# ------------------------------------------------------------------------------
# Plotting a binary response with a smoothed, non-parametric curve
# ------------------------------------------------------------------------------
graphics.off()
xvalues <- seq(15, 85, 5)
pred.logistic <- predict(arth.logistic, newdata=data.frame(Age=xvalues), type="response", se.fit=TRUE)
upper <- pred.logistic$fit + 1.96*pred.logistic$se.fit
lower <- pred.logistic$fit - 1.96*pred.logistic$se.fit



# ----------
# by base graphics
plot(jitter(Better, .1) ~ Age, data = data, xlim = c(15, 85), pch = 16, ylab = "Probability (Better)")
polygon(c(xvalues, rev(xvalues)), c(upper, rev(lower)), col=rgb(0,0,1,.2), border=NA)
lines(xvalues, pred.logistic$fit, lwd = 4 , col = "blue")
abline(arth.lm, lwd = 2)
lines(lowess(data$Age, data$Better, f = .9), col = "red", lwd = 2)


# --> 
# Even though the linear probability model is inappropriate theoretically, this give similar predicted probability to
# those of the logistic model between age 25-75, where most of the data points are located.



# ----------
# by ggplot
gg <- ggplot(data, aes(x = Age, y = Better)) + xlim(5, 95) + geom_point(position = position_jitter(height = 0.02, width = 0)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), alpha = 0.1, fill = "blue", size = 2.5, fullrange = TRUE)
gg <- gg + stat_smooth(method = "loess", se = FALSE, span = 0.95, colour = "red", size = 1.2)
gg



