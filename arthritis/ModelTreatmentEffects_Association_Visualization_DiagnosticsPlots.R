# ------------------------------------------------------------------------------
# Model Treatment Effect for Arthristis patient
#
# Visualizing data and diagnostics plots
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "popbio", "effects")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- Arthritis
str(data)

describe(data)


# ------------------------------------------------------------------------------
# Generate binary response
# ------------------------------------------------------------------------------
data$Better <- as.numeric(data$Improved > "None")


# ------------------------------------------------------------------------------
# Basic Analysis: Treatment is effective ?
#
# Odds ratio, log odds ratio
# significance test and 95% confidence interval
# ------------------------------------------------------------------------------
str(data$Improved)

( tmp <- xtabs(~ Treatment + Improved, data = data) )
round(prop.table(tmp, margin = 1), 4)
( tmp2 <- xtabs(~ Treatment + Better, data = data) )


# "Treated" seems to have more some and marked improvement compared to "Placebo"
spineplot(tmp)

# Fourfold display for 2 * 2 tables
# standardize both margins are equal, while preserving odds ratio
fourfold(tmp2)
fourfold(tmp2, std="ind.max")  # unstandardized


# Mosaic plot with pearson residuals
mosaic(tmp, gp=shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digits = 2)

set.seed(1234)
mosaic(tmp, gp=shading_max, margin=c(right = 1))

interp <- function(x) pmin(x / 6, 1)
mosaic(tmp, shade = TRUE, gp_args = list(interpolate = interp))



# standard Chi^2 tests amd measure of association
assocstats(tmp)


# CMH tests (Cochran-Mantel-Haenszel tests):  generally have higher power when the pattern of association is determined by the order of an ordinal variable
CMHtest(tmp)


# Odds Ratio for Treatment and Better  -->  4.461538
# Log Odds Ratios for Treatment and Better  --> 1.495494
( OR <- loddsratio(tmp2, log = FALSE) )
( LOR <- loddsratio(tmp2, log = TRUE) )


# Significance Test for Log Odds Ratio
summary(LOR)


# 95% confidence interval
confint(OR)
confint(LOR)


# Almost same result:  odds ratios and its 95% confidence interval
fisher.test(tmp2)


# ------------------------------------------------------------------------------
# Stratified Analysis
# ------------------------------------------------------------------------------
( tmp_s <- xtabs(~ Treatment + Improved + Sex, data = data) )
( tmp_s_m <- xtabs(~ Treatment + Improved, data = data, subset = Sex == "Male") )
( tmp_s_f <- xtabs(~ Treatment + Improved, data = data, subset = Sex == "Female") )
( tmp_s2 <- xtabs(~ Treatment + Better + Sex, data = data) )


# fourfold 2 * 2 * k tables
fourfold(tmp_s2)


# Mosaic plot with pearson residuals
mosaic(tmp_s_m, gp=shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digits = 2)
mosaic(tmp_s_f, gp=shading_Friendly, margin = c(right = 1), labeling = labeling_residuals, suppress = 0, digits = 2)

set.seed(1234)
mosaic(tmp_s_m, gp=shading_max, margin=c(right = 1))
mosaic(tmp_s_f, gp=shading_max, margin=c(right = 1))


# standard Chi^2 test and CMHtest
# NOTE THAT:  even though the strength of association (Cramer's V) is similar in the two groups(Male and Female), 
# the Chi^2 tests show significance for females
# This is true even using the more powerful CMH test
assocstats(tmp_s)
CMHtest(tmp_s)


# There are more than twice as many females as males in this sample.  -->  small sample is the reason why not significant for males ???
apply(tmp_s, 3, sum)


# 
( OR <- loddsratio(tmp_s, log = FALSE) )
( LOR <- loddsratio(tmp_s, log = TRUE) )
confint(OR)
confint(LOR)


# Assessing homogeneity of association by woolf test:  H0:  odds ratios cannot be considered equal between Male and Female
# Even though we found in the CMHtest that the association between Treatment and Improved was stronger for female than males,
# the analysis using woolf_test is clearly non-significant, so we cannot reject homogeneity of association
woolf_test(tmp_s)


# Assessing homogeneity of association by loglinear model of no three-way association
# the result is consistent with woolf test (interaction term is not signicitant)
library(MASS)
loglm(~ (Treatment + Improved + Sex)^2, data = tmp_s)


# ------------------------------------------------------------------------------
# Univariate logistic regression and its binary responses distribution
# ------------------------------------------------------------------------------
# Continuous variable vs Y
with(data, 
     popbio::logi.hist.plot(independ = Age, depend = Improved > "None", 
                            logi.mod = 1, # logistic
                            type="hist", counts=TRUE, 
                            boxp = TRUE, rug = TRUE,
                            ylabel = "Probability (Better)", xlabel = "Age",
                            col.hist = "lightblue"))


# ------------------------------------------------------------------------------
# For predictors:  Marginal Plots
# ------------------------------------------------------------------------------
op <- mar(mfrow(c(2,2), mar = c(4,4,1,2.5) + .1 , cex.lab = 1.4))

col <- c("lightblue", "pink")
plot(Better ~ Age, data = data, col = col)
plot(Better ~ Treatment, data = data, col = col)
plot(Better ~ Sex, data = data, col = col)



# ------------------------------------------------------------------------------
# Use rms::plot.xmean.ordinaly
# Solid line:  simple sratified means
# Dashed line:  connect the estimated expected value of X|Y = j given that PO holds
# Estimated expexted values from the CR(Continuous Ratio) model are amrked with Cs
# ------------------------------------------------------------------------------
plot.xmean.ordinaly(Improved ~ Treatment + Sex + Age, data = data, cr=TRUE, subn = TRUE, cex.points=0.65)


# ------------------------------------------------------------------------------
# Simple binary logistic regression by glm
# ------------------------------------------------------------------------------
modglm <- glm(Better ~ Age, data = data, family = binomial)
summary(modglm)

# How much better is the fitted model than the null model (only intercept) ?
anova(modglm, test = "Chisq")

# How bad is the fitted model, compared to a saturated model ?  --> significantly showing lack of fit
library(vcdExtra)
LRstats(modglm)


# ------------------------------------------------------------------------------
# Simple binary logistic regression by lrm
# ------------------------------------------------------------------------------
modlrm <- lrm(Better ~ Age, data = data)

print(modlrm)
modlrm$stats

# Wald-ANOVA table
an <- anova(modlrm)
print(an)


# CHECK Optimistic level
# van Houwelingen-Le Cessie heuristic shrinkage estimation
# "1 - gamma.hat":  this model will validate on new data about X% worse than on this dataset.
# this can be shown to equal = { (n - p - 1) / (n - 1) } * { R^2adjusted / R^2 }
( s <- modlrm$stats )
( gamma.hat <- (s["Model L.R."] - s["d.f."]) / s["Model L.R."] )
( 1 - gamma.hat )


# Partial Effects (log odds scale)
dd <- datadist(data);  options(datadist="dd")
ggplot(Predict(modlrm), sepdiscrete="vertical", vnames="names", rdata=data, histSpike.opts=list(frac=function(a) .1 * a/max(a)))


# Odds Ratios
# interquartile-range odds ratios for continuout predictors and simple odds ratios for categorical predictors
# the bars represent 0.9, 0.95, 0.99
# numbers at left are upper quartile and lower quartile, or current group
plot(summary(modlrm), log=TRUE)


plot(nomogram(modlrm))


# ------------------------------------------------------------------------------
# Plotting a binary responses
# ------------------------------------------------------------------------------
# basic way
xvalues <- seq(15,85,5)
pred <- predict(modglm, newdata = data.frame(Age = xvalues), type = "response", se.fit = TRUE)
upper <- pred$fit + 1.96 * pred$se.fit
lower <- pred$fit - 1.96 * pred$se.fit

plot(jitter(Better, .1) ~ Age, data = data, xlim=c(15,85), pch = 16, ylab = "Probability (Better)")
polygon(c(xvalues, rev(xvalues)), c(upper, rev(lower)), col = rgb(0,0,1,.2), border = NA)
lines(xvalues, pred$fit, lwd=4, col="blue")
lines(lowess(data$Age, data$Better, f = 0.9), col="red", lwd=2)


# ggplot2  -->  logistic regression and plotting simultaneously
gg <- ggplot(data, aes(x = Age, y = Better)) + xlim(5,95) + geom_point(position = position_jitter(height = 0.02, width = 0))
gg <- gg + stat_smooth(method = "glm", method.args=list(family = "binomial"), alpha=0.1, fill = "blue", size = 2.5, fullrange = TRUE)
gg <- gg + stat_smooth(method = "lm", se = FALSE, size = 1.2, color = "black", fullrange = TRUE)
gg <- gg + stat_smooth(method = "loess", se = FALSE, span = 0.95, size = 1.2, color = "red")
gg


# ------------------------------------------------------------------------------
# Conditional Plots
#
# Logistic Regression and Plotting Simultanously
# ------------------------------------------------------------------------------
gg <- ggplot(data, aes(x = Age, y = Better, color = Treatment)) + xlim(5,95) + geom_point(position = position_jitter(height = 0.02, width = 0)) + theme_bw()
gg <- gg + stat_smooth(method = "glm", method.args=list(family = "binomial"), alpha=0.2, aes(fill = Treatment), size = 2.5, fullrange = TRUE)

gg
gg + facet_wrap(~ Sex)

# less than 1/3 of the sample are males, and of these only 11 are in the placebo group. --> glm() cannot estimate the fitted relationship again Age here.
addmargins(xtabs(~ Sex + Treatment, data  = data), 2)


# ------------------------------------------------------------------------------
# Full-Model Plots
#
# Logistic Regression and Plotting Simultanously
# ------------------------------------------------------------------------------
modglm2 <- glm(Better ~ I(Age - 50) + Sex + Treatment, data = data, family=binomial)

vcd::binreg_plot(modglm2, type = "link")
vcd::binreg_plot(modglm2)

vcd::binreg_plot(modglm2, type = "link", subset = Sex == "Female", main = "Female", xlim=c(25,75), ylim = c(-3,3))
vcd::binreg_plot(modglm2, type = "link", subset = Sex == "Male", main = "Male", xlim=c(25,75), ylim = c(-3,3))

vcd::binreg_plot(modglm2, subset = Sex == "Female", main = "Female", xlim=c(25,75))
vcd::binreg_plot(modglm2, subset = Sex == "Male", main = "Male", xlim=c(25,75))


# ------------------------------------------------------------------------------
# Full model's Effect and Residual Plots
# ------------------------------------------------------------------------------
modglm2.eff <- allEffects(modglm2, partial.residuals = TRUE)

# main effect for "Sex" and its model matrix
modglm2.eff[["Sex"]]
modglm2.eff[["Sex"]]$model.matrix


# plot all effects in the main effects model
# Partial residuals adn their loess smooth are also shown for the continuous predictor --> residual plot show a hint of nonlinearity
plot(modglm2.eff, rows = 1, cols = 3, type="response", residuals.pch = 15)


# Effect plot by joint predictors by logit scale
modglm.full <- Effect(c("Age", "Treatment", "Sex"), modglm2)
plot(modglm.full, multiline = TRUE, ci.style = "bands",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)),
     key.args = list(x = .52, y = .92, columns = 1),
     grid = TRUE)


# Effect plot by joint predictors by probability scale
plot(modglm.full, multiline = TRUE, ci.style = "bands",
     type = "response",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)),
     key.args = list(x = .52, y = .92, columns = 1),
     grid = TRUE)



# ------------------------------------------------------------------------------
# Influence measures
# ------------------------------------------------------------------------------
infl <- influence.measures(modglm2)

# potential influential observation
summary(infl)

# Influence Plot (by car package):  residual vs leverage,  showing Cook's D as the size of the bubble symbol
# Note that in OLS, the hat values depend only on the X's, whereas in logistic regression, they also depend on the dependent variable values and the fitted probabilities.
# As a result, an observation may be extremely unusual on the predictors, yet not have a large hat value, if the fitted probability is near 0 or 1
op <- par(mar = c(5,4,1,1) + .1, cex.lab = 1.2)
res <- car::influencePlot(modglm2, id.col = "blue", scale = 8, id.n = 2)
k <- length(coef(modglm2))
n <- nrow(data)
text(x = c(2,3) * k / n, y = -1.8, c("2k/n", "3k/n"), cex = 1.2)


# observation with potential impact
res
data[rownames(res),]


# show each observation's influence measure
cnt <- 4  # show most extreme observations individually for each measure plotted
car::influenceIndexPlot(modglm2, vars=c("Cook", "Studentized", "Hat"), id.n = cnt)


# DFBETA:  standardized change in the coefficient for each variable due to omitting that observation.
# pairwise changes in the regression coefficients for the various predictors by scatterplotmatrix
dfbetas <- data.frame(infl$infmat[,2:4])
dfbetas

car::scatterplotMatrix(dfbetas, smooth=FALSE, id.n = cnt, ellipse = TRUE, levels = 0.95, robust = FALSE, diagonal = "histogram", 
                  groups = data$Better, col = c("blue", "red"))


# ------------------------------------------------------------------------------
# Component-plus-residual plots (partial residual plot)
# 
# check nonlinear relation, requiring transformation
# useful when there are several quantitative predictors
# ------------------------------------------------------------------------------
# show no indication of a nonlinear relationship
# Dashed red line:  the slope of I(Age-50) in the model
# Smoothed green curve:  loess fit with span = 0.5
car::crPlots(modglm2, ~I(Age-50), id.n = 4)



# ------------------------------------------------------------------------------
# Added-variable plots (partial regression plots)
# 
# show the conditional relationship between the response and the predictor, controlling for, or adjusting for, all other predictors.
# make jointly influential points visually apparent
# ------------------------------------------------------------------------------
col <- ifelse(data$Better == 1, "blue", "red")
pch <- ifelse(data$Sex == "Male", 16, 17)
car::avPlots(modglm2, id.n = 4, col = col, pch = pch, col.lines = "darkgreen")



