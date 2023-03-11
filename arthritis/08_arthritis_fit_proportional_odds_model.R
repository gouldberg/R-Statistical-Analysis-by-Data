setwd("//media//kswada//MyFiles//R//arthritis")

packages <- c("dplyr", "vcd", "MASS", "ggplot2", "VGAM")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arthritis
# ------------------------------------------------------------------------------
data("Arthritis", package = "vcd")

data <- Arthritis

data


# check the "Improved" as an ordered factor
head(data$Improved)




# ------------------------------------------------------------------------------
# Fitting the proportional odds model
# ------------------------------------------------------------------------------
# Hess = TRUE to have the function return the observed information matrix (Hessian), that is used in other operations to calculate standard errors.
# summary() does not provide no significant tests or p-values
arth.polr <- MASS::polr(Improved ~ Sex + Treatment + Age, data = data, Hess = TRUE)
summary(arth.polr)



# ----------
# car::Anova() method gives the significance tests and p-values
car::Anova(arth.polr)



# ------------------------------------------------------------------------------
# Testing the proportional odds assumption
#   - In R, the proportioanl odds model (PO model) and non-proportional odds model (NPO model) can be readily contrasted by fitting them
#     both using vglm() in the VGAM package
# ------------------------------------------------------------------------------
# parallel = TRUE, this is equivalent to the polr() model, except that the signs of the coefficients are reserved.
arth.po <- VGAM::vglm(Improved ~ Sex + Treatment + Age, data = data, family = cumulative(parallel = TRUE))
summary(arth.po)



# ----------
# The more general Non-Proportional Model
arth.npo <- VGAM::vglm(Improved ~ Sex + Treatment + Age, data = data, family = cumulative(parallel = FALSE))
summary(arth.npo)



# ----------
# Also fit partial proportional odds models
# by specifying a formula giving the terms for which the PO assumption should be taken as TRUE or FALSE
# parallel = FALSE ~ Sex, to fit separate slopes for males and females, but parallel lines for the other predictors
( arth.ppo <- VGAM::vglm(Improved ~ Sex + Treatment + Age, data = data, family = cumulative(parallel = FALSE ~ Sex)) )
coef(arth.ppo, matrix = TRUE)



# ----------
# VGAM packages defines a coef() method that can print the coefficients in a more readable matrix form giving the category cutpoints
coef(arth.po, matrix = TRUE)
coef(arth.npo, matrix = TRUE)



# ----------
# In most cases, nested models can be test using an anova() method, but the VGAM package has not implemented this for "vglm" objects.
# Instead, it provides an analogous function lrtest()
VGAM::lrtest(arth.npo, arth.po)



# ----------
# The LR test can also be calculated manually using the difference in residual deviance for the two models.
tab <- cbind(
  Deviance = c(deviance(arth.npo), deviance(arth.po)),
  df = c(df.residual(arth.npo), df.residual(arth.po))
)
tab <- rbind(tab, diff(tab))
rownames(tab) <- c("GenLogit", "PropOdds", "LR test")
tab <- cbind(tab, pvalue=1-pchisq(tab[,1], tab[,2]))
tab



# ------------------------------------------------------------------------------
# Graphical asessment of proportional odds
#  - plot of conditional X means
# ------------------------------------------------------------------------------
arth.po2 <- rms::lrm(Improved ~ Sex + Treatment + Age, data = data)
arth.po2



# ----------
# Plot of conditional X means, producing one marginal panel for each predictor in the model.
# For categorical predictors, it plots only the overall most frequent category
# Solid lines connect the stratified means of X given Y.
# Dashed lines show the estimated expected value of X given Y = j if the proportional odds model holds for X
op <- par(mfrow=c(1,3))
rms::plot.xmean.ordinaly(Improved ~ Sex + Treatment + Age, data=data, lwd=2, pch=16, subn=FALSE)
par(op)


# --> There is some evidence that the effect of Sex is non-monotonic and the means differ from their model-implied values under the PO assumption
# The effect of Treatment looks good by this method, and the effect of Age hints that upper two categories may not be wel-distinguished as an ordinal response.



# ------------------------------------------------------------------------------
# Partial proportional odds models
# by specifying a formula giving the terms for which the PO assumption should be taken as TRUE or FALSE
# ------------------------------------------------------------------------------
# parallel = FALSE ~ Sex, to fit separate slopes for males and females, but parallel lines for the other predictors
( arth.ppo <- VGAM::vglm(Improved ~ Sex + Treatment + Age, data = data, family = cumulative(parallel = FALSE ~ Sex)) )
coef(arth.ppo, matrix = TRUE)



# ------------------------------------------------------------------------------
# Visualizing results for the proportional odds model
# Full-model plots
# ------------------------------------------------------------------------------
arth.fitp <- cbind(data, predict(arth.polr, type = "probs"))
head(arth.fitp)


# Reshape from wide to long, response category is named Level
library(reshape2)
plotdat <- reshape2::melt(arth.fitp, id.vars = c("Sex", "Treatment", "Age", "Improved"), measure.vars = c("None", "Some", "Marked"),
                          variable.name = "Level", value.name = "Probability")
head(plotdat)


# Predicted probabilities for the proportional odds model
gg <- ggplot(plotdat, aes(x = Age, y = Probability, colour = Level)) + geom_line(size = 2.5) + theme_bw() + xlim(10, 80) + 
  geom_point(color = "black", size = 1.5) + facet_grid(Sex ~ Treatment)
gg

# gg <- ggplot(plotdat, aes(x = Age, y = Probability, colour = Level)) + geom_line(size = 2.5) + theme_bw() + xlim(10, 80) + geom_point(color = "black", size = 1.5) +
#  facet_grid(Sex ~ Treatment, labeller = function(x, y) sprintf("%s = %s", x, y))
#direct.label(gg)


# --> In each panel, the probability of no improvement decreases with age, while that for marked improvemnet increases.
# It is easy to compare the placebo and treated groups in each row, showing that no improvement decreases, while marked improvement increases with the active treatment.
# The points show where the observations are located in each panel; data is quite thin for males given placebo



# ------------------------------------------------------------------------------
# Visualizing results for the proportional odds model
# Effect plots
# ------------------------------------------------------------------------------
# By default, curves are plotted in separate panels for the different response levels of a given effect, together with confidence bands for predicted probabilities
plot(effects::Effect("Age", arth.polr))


# Stacked format shows the changes in response level more directly, but does not provide confidence bands
plot(effects::Effect("Age", arth.polr), style = "stacked", key.args = list(x = .55, y = .9))


# predicted probabilities for all three predictors together
plot(effects::Effect(c("Treatment", "Sex", "Age"), arth.polr), style = "stacked", key.arg = list(x = .8, y = .9))


# Latent variable effect plot
# In this plot, there is a single line in each panel for the effect (slope) of Age on the log odds.
# Dashed horizontal lines give the thresholds between the adjacent response categories corresponding to the intercepts.
plot(effects::Effect(c("Treatment", "Age"), arth.polr, latent = TRUE), lwd = 3)


