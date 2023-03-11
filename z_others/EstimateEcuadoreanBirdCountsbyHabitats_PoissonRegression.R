# ------------------------------------------------------------------------------
# Estimate mean count of bird captures at each location as an index of the overall abundance, and compare these means across different types of habitats
# Explanatory variables are only categorical 
# 
# "mcprofile" package:
# The package mcprofile contains functions for simultaneous inference based on a set of profile statistics, 
# controlling the family-wise error rate (FWER) at a specified level. 
# Focus is set on testing general linear hypotheses in generalized linear models and provide a set of compatible simultaneous confidence intervals. 
# The multiple testing framework of Hothorn et al. (2008) is adopted to signed likelihood root and modified likelihood root statistics to improve the performance
# especially at small sample sizes.


# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")
packages <- c("dplyr", "faraway", "Hmisc", "lattice", "ggplot2", "corrplot", "GGally", "psych")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.table(file="./AnalysisOfCategoricalDataWithR/Chapter4/BirdCounts.csv", sep=",", header=T, stringsAsFactors=TRUE)

str(data)

# check categorical variable design
contrasts(data$Loc)


# boxplot and dotplot by ggplot  --> sample data is very small...
ggplot(data, aes(y=Birds, x=Loc)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.2) +    
  xlab("Loc")

dotplot(Birds ~ Loc, data = data)


# ------------------------------------------------------------------------------
# Poisson Regression
# Test whether all means are equal and get confidence intervals in count scale
# ------------------------------------------------------------------------------
# Fit Poisson Regression to estimate mean counts for each habitat
# estimate mean counts
M1 <- glm(Birds ~ Loc, family = poisson(link = "log"), data = data)
summary(M1)

# LRT:  test whether all means are equal
anova(M1, test = "Chisq")
car::Anova(M1, test = "LR")

# Get predicted means and CIs
pred.data <- data.frame(Loc = c("ForA", "ForB", "Frag", "Edge", "PasA", "PasB"))
means <- predict(object = M1, newdata = pred.data, type = "link", se.fit = TRUE)

# Wald CI for log means
alpha <- 0.05
lower.logmean <- means$fit + qnorm(alpha/2)*means$se.fit
upper.logmean <- means$fit + qnorm(1 - alpha/2)*means$se.fit

# Combine means and confidence intervals in count scale
mean.wald.ci <- data.frame(pred.data, round(cbind(exp(means$fit), exp(lower.logmean), exp(upper.logmean)), digits = 2))
colnames(mean.wald.ci) <- c("Location", "Mean", "Lower", "Upper")
mean.wald.ci

# Alternative parameterization for model to get LR intervals for means
# "-1" in formula removes intercept and causes model to estimate log-means directly 
M1.rp <- glm(formula = Birds~ Loc - 1, family = poisson(link = "log"), data = data)
LR.ci <- exp(confint(M1.rp))
mean.LR.ci2 <- data.frame(Estimate = exp(M1.rp$coefficients), Lower = LR.ci[,1], Upper = LR.ci[,2])
mean.LR.ci2


# ------------------------------------------------------------------------------
# LR intervals for means
#
# Plot shows that general increasing trend in the mean counts as the cmount of forestation increases
# Also it is apparent that one forest location has a mean that is drastically different from the other locations
# ------------------------------------------------------------------------------
# Get LR intervals for means.  Each log-mean is a combination of intercept and a parameter.
# Want means ordered by level of forestation: Forests, Fragment, Edge, Pastures 
 
library(mcprofile)
K <- matrix(data = c(1, 1, 0, 0, 0, 0,
                     1, 0, 1, 0, 0, 0,
                     1, 0, 0, 1, 0, 0,
                     1, 0, 0, 0, 0, 0,
                     1, 0, 0, 0, 1, 0,
                     1, 0, 0, 0, 0, 1), nrow = 6, ncol = 6, byrow = TRUE)
K

# Profile LR
linear.combo <- mcprofile(object = M1, CM = K)
ci.log.mu <- confint(object = linear.combo, level = 0.95, adjust = "none")

# plot
plot(linear.combo)
plot(ci.log.mu)


mean.LR.ci1 <- data.frame(Loc = pred.data, Estimate = exp(ci.log.mu$estimate), Lower = exp(ci.log.mu$confint[,1]), Upper = exp(ci.log.mu$confint[,2]))
mean.LR.ci1

mean.LR.ci1$Loc2 <- factor(mean.LR.ci1$Loc, levels = levels(mean.LR.ci1$Loc)[c(2,3,4,1,5,6)])

# StripChart in color
x11(width = 7, height = 5, pointsize = 12) 
# pdf(file = "c:\\figures\\Figure4.7color.pdf", width = 7, height = 5, colormodel = "cmyk")   # Create plot for book
stripchart(Lower ~ Loc2, data = mean.LR.ci1, vertical = FALSE, xlim = c(20,150), col = "red", pch = "(", main = "", xlab = "Bird Count", ylab = "Location")
stripchart(Upper ~ Loc2, data = mean.LR.ci1, vertical = FALSE, col = "red", pch = ")", add = TRUE)
stripchart(Estimate ~ Loc2, data = mean.LR.ci1, vertical = FALSE, col = "red", pch = "+", add = TRUE)
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
abline(v = mean(data$Birds), col = "darkblue", lwd = 4)
# dev.off()




# ------------------------------------------------------------------------------
# Wald Intervals
# Plots based on Wald Intervals
# ------------------------------------------------------------------------------
# Redefining location group factor so that plot does not alphabetize
mean.wald.ci$Loc2 <- factor(mean.wald.ci$Location, levels = c("ForA", "ForB", "Frag", "Edge", "PasA", "PasB"))
mean.wald.ci

# StripChart in color
x11(width = 7, height = 5, pointsize = 12)
# NOTE: vertical = FALSE shows intervals horizontally
stripchart(Lower ~ Loc2, data = mean.wald.ci, vertical = FALSE, xlim = c(20,150), col = "red", pch = "(", main = "", xlab = "Bird Count", ylab = "Location")
stripchart(Upper ~ Loc2, data = mean.wald.ci, vertical = FALSE, col = "red", pch = ")", add = TRUE)
stripchart(Mean ~ Loc2, data = mean.wald.ci, vertical = FALSE, col = "red", pch = "+", add = TRUE)
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
abline(v = mean(data$Birds), col = "darkblue", lwd = 4)


# ------------------------------------------------------------------------------
# Compare two habitations in pair
# Confidence intervals for linear combinations of parameters
# ------------------------------------------------------------------------------
# Create coefficients for Lin Combos
contr.mat <- rbind(c(0,.5,.5,0,0,0), c(0,.5,.5,-1,0,0), c(0,.5,.5,0,-.5,-.5), 
                   c(0,0,0,1,0,0), c(0,0,0,1,-.5,-.5), c(0,0,0,0,-.5,-.5))
rownames(contr.mat) <- c("For-Edge", "For-Frag", "For-Past", "Frag-Edge", "Frag-Past", "Edge-Past")
contr.mat

# Wald inferences using multcomp package
library(multcomp)
loc.test <- glht(model = M1, linfct = contr.mat)

# Defaults use multiplicity adjustment for simultaneous confidence level
summary(loc.test)
exp(confint(loc.test)$confint)
# Options to get unadjusted (univariate) tests and CIs
summary(loc.test, test = univariate())
exp(confint(loc.test, calpha = qnorm(0.975))$confint)

# Do the same thing with LR in mcprofile
linear.combo <- mcprofile(object = M1, CM = contr.mat)
summary(linear.combo)  
exp(confint(linear.combo)$confint)

# No adjustment for simultaneous comparison since we would like to get not conservative result for future investigation
summary(linear.combo, adjust = "none")
exp(confint(linear.combo, adjust = "none")$confint)


plot(linear.combo)
plot(confint(linear.combo, adjust = "none"))


# ------------------------------------------------------------------------------
# Wald CIs by manual computation
# ------------------------------------------------------------------------------
# Get out coefficients and variances
beta <- matrix(coef(M1), ncol = 1)
v.beta <- vcov(M1)

# Estimate Lin Combos and standard errors as matrix computations
log.contrasts <- contr.mat %*% beta
SElog.contrasts <- matrix(sqrt(diag(contr.mat %*% v.beta %*% t(contr.mat))), ncol = 1)
log.contrasts
SElog.contrasts

# Compute confidence intervals in linear predictor scale
alpha <- 0.05
lower.log <- log.contrasts + qnorm(alpha/2)*SElog.contrasts
upper.log <- log.contrasts + qnorm(1-alpha/2)*SElog.contrasts
# Combine Lin Combo coefficients, estimates of contrasts, and confidence intervals in mean scale
wald.ci <- round(cbind(exp(log.contrasts), exp(lower.log), exp(upper.log)), digits = 2)
colnames(wald.ci) <- c("Estimate", "Lower", "Upper")
wald.ci








