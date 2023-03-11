# ------------------------------------------------------------------------------
# Data:  Arthritis 
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "popbio", "effects", "MASS", "car")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- Arthritis
str(data)

psych::describe(data)
Hmisc::describe(data)

car::some(data)


# Check zero frequency cells  (potential impact to logistic regression's predictive performance)
dat.tab <- xtabs(~ Treatment + Improved + Sex + Age, data = data)
ftable(dat.tab)

ftable(dat.tab, row.vars = c(1,2))


# Ordered factor
data$Improved <- ordered(data$Improved, levels = c("None", "Some", "Marked"))


# ------------------------------------------------------------------------------
# Overview distribution of each variable and correlation among variables
# ------------------------------------------------------------------------------
library(gpairs)
library(vcd)

var <- c("Improved", "Treatment", "Sex", "Age")

# marical and conditional plots
gpairs(data[,var],
       diag.pars = list(fontsize = 16, hist.color ="lightgray",
                        mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate=1:4)),
                        outer.rot = c(45,45)))


# check correlation
psych::pairs.panels(data[, var])


# ------------------------------------------------------------------------------
# The relationship of numeric variables X against Y
# Margianl plots
# ------------------------------------------------------------------------------
num_var <- c("Age")

psych::pairs.panels(data[, c(num_var, "Improved")])


# Margianl plots to check relationship between y and x
op <- par(mfrow(c(2,2), mar = c(4,4,1,2.5) + .1 , cex.lab = 1.4))
col <- c("lightblue", "blue", "salmon")
par(mfrow=c(1,1))
plot(Improved ~ Age, data = data, col = col)


# ------------------------------------------------------------------------------
# Association of each categorical and factor variable X against Y
# doubledecker
# ------------------------------------------------------------------------------
catfac_var <- c("Treatment", "Sex")

# doubledecker
for(i in 1:length(catfac_var)){
  eval(parse(text = paste0("vcd::doubledecker(Improved ~ ", catfac_var[i], ", data = data)")))
}

# Female > Male
# Treated > Placebo


# ------------------------------------------------------------------------------
# Significant association of each binary variables X against Y
# fourfold display (+ confidence interval of odds ratio)
# ------------------------------------------------------------------------------
bin_var <- c("Treatment", "Sex")

data <- data %>% mutate(Improved2 = ifelse(Improved %in% c("Some", "Marked"), "Improved", "None"))
data$Improved2 <- ordered(data$Improved2, levels = c("None", "Improved"))

# fourfold display
for(i in 1:length(bin_var)){
  eval(parse(text = paste0("tmp <- xtabs(~ Improved2 + ", bin_var[i], ", data = data)")))
  fourfold(tmp, fontsize = 16)
}


# ------------------------------------------------------------------------------
# Significant association of ordinal variables X against Y
# local odds ratio
# ------------------------------------------------------------------------------
# Local odds ratio is not significant for polytonomous responses but significant for binary response !!!

confint(loddsratio(Improved ~ Treatment, data = data, log=TRUE), level = 0.95)
confint(loddsratio(Improved2 ~ Treatment, data = data, log=TRUE), level = 0.95)

confint(loddsratio(Improved ~ Sex, data = data, log=TRUE), level = 0.95)
confint(loddsratio(Improved2 ~ Sex, data = data, log=TRUE), level = 0.95)


# ------------------------------------------------------------------------------
# Test for the significance of coefficients
# Univariale logistic regression
# ------------------------------------------------------------------------------
source("./utilities/UnivarLogiReg.R")

x_var <- c("Treatment", "Sex", "Age")
y_var <- "Improved"

crit <- c(0.25, 0.05, 0.001)
uni_lr <- UnivarLogiReg(y_var = y_var, x_var = x_var, data = data, crit = crit)
uni_lr


# Check how bad is this univariate model, compared to a model (saturated model) that fits the data perfectly ?
i <- 1
x_var[i]
eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", x_var[i], ", data = data, family = binomial)")))
vcdExtra::LRstats(mod)


# ------------------------------------------------------------------------------
# Multivariable model with all covariates
# ------------------------------------------------------------------------------
source("./utilities/MultivarLogiReg.R")

x_var <- c("Treatment", "Sex", "Age")
y_var <- "Improved"
crit <- c(0.05, 0.001)

mult_lr <- MultivarLogiReg(y_var = y_var, x_var = x_var, data = data, crit = crit)

mult_lr["mult_lr"]
mult_lr["G.stat"]


# get model object
mod_char <- paste0(x_var, collapse = " + ")
eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
summary(mod)


# effect plot
eff <- effects::allEffects(mod, partial.residuals = TRUE)
# plot main effects and partial residuals for numeric variables with smoothed loess curve
plot(eff, rows = 2, cols = 2, type = "response", residuals.pch = 15)


# ------------------------------------------------------------------------------
# Graphic Assessment of Proportional Odds Assumption
# 
# Plot conditional X means or expected valuye E(X|Y) of a given predictor, X, at each level of the ordered response Y.
# For comparison, one can also plot hte estimated conditional means E-hat(X|Y= j) under the fitted PO model X as the only predictor.
# If the PO assumption holds for this X, the model-mean curve should be close to the data mean curve.
# ------------------------------------------------------------------------------
dd <- datadist(data);  options(datadist="dd")

modlrm <- rms::lrm(Improved ~ Sex + Treatment + Age, data = data)
modlrm
plot(summary(modlrm))


# There is some evidence that the effect of Sex is non-monotonic and the meas differ from their model-implied values under the PO assumption.
# The effect of Treatment looks good by this method, and the effect of Age hints that the upper tow categories may not be well-distinguished as an ordinal response.
# CRモデルでの期待値が "C" でマーク
op <- par(mfrow=c(1,3))
plot.xmean.ordinaly(Improved ~ Sex + Treatment + Age, data = data, cr = TRUE, lwd = 1, pch = 16, subn = TRUE, cex.points=.65)
par(op)


# ------------------------------------------------------------------------------
# Numerical Assessment of Proportional Odds Assumption
# ------------------------------------------------------------------------------
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

# the (linear) predicted values (logit) if we regressed our dependent variable on our predictor variables one at a time, without the parallel slopes assumption. 
(s <- with(data, summary(as.numeric(Improved) ~ Sex + Treatment + Age, fun=sf)))


# Doing same ...
glm(I(as.numeric(Improved) >= 2) ~ Treatment, family="binomial", data = data)
glm(I(as.numeric(Improved) >= 3) ~ Treatment, family="binomial", data = data)


# For Treatment, (Y>=3) - (Y=2) is almost equal between Placebo and Treated  -->  parallel slopes assumption is reasonable
# Placebo: -1.64 - (-0.76) = -0.88
# Treated: 0.05 - 0.77 = -0.72

s[,4] <- s[,4] - s[,3]
s[,3] <- s[,3] - s[,3]
s

par(mfrow=c(1,1))
plot(s, which = 1:3, pch = 1:3, xlab = "logit", main="", xlim = range(s[,3:4]))


# ------------------------------------------------------------------------------
# Fitting the Proportional Odds Model (by polr())
# ------------------------------------------------------------------------------
# specify HEss = True to have the function return the observed information matrix (hessian), that is used in other operations to calculate standard errors
modpo_polr <- MASS::polr(Improved ~ Sex + Treatment + Age, data = data, Hess = TRUE, method = "logistic")

# No pvalue so ...
summary(modpo_polr)
# test for coefficients and intercept
car::Anova(modpo_polr)


# Wald statistic7s p.val for each coefficients
m1.coef <- data.frame(coef(summary(modpo_polr)))
m1.coef$pval <- round((pnorm(abs(m1.coef$t.value), lower.tail= FALSE) * 2),4)
m1.coef

                     
# ------------------------------------------------------------------------------
# Proportional Odds Model,  Non-Proportional Odds Model,  Partial Proportional Odds Model
# ------------------------------------------------------------------------------
modpo_vglm <- VGAM::vglm(Improved ~ Sex + Treatment + Age, data = data, family = cumulative(link = "logit", parallel = TRUE))
summary(modpo_vglm)
coef(modpo_vglm, matrix = TRUE)


modnpo_vglm <- VGAM::vglm(Improved ~ Sex + Treatment + Age, data = data, family = cumulative(link = "logit", parallel = FALSE))
summary(modnpo_vglm)
coef(modnpo_vglm, matrix = TRUE)

modppo_vglm <- VGAM::vglm(Improved ~ Sex + Treatment + Age, data = data, family = cumulative(link = "logit", parallel = FALSE ~ Sex))
summary(modppo_vglm)
coef(modppo_vglm, matrix = TRUE)


# anova:  VGAM packages has not implemented avova() method for vglm object.  Uses lrtest()
VGAM::lrtest(modpo_vglm, modnpo_vglm)
VGAM::lrtest(modpo_vglm, modppo_vglm)
VGAM::lrtest(modnpo_vglm, modppo_vglm)


# AIC
AIC(modpo_vglm);  AIC(modnpo_vglm);  AIC(modppo_vglm)


# ------------------------------------------------------------------------------
# Visualizing results for the proportional odds model
# ------------------------------------------------------------------------------
# proportioal odds model
mod <- modpo_polr
tmp <- cbind(data, predict(mod, type="probs"))

# non proportioal odds model
mod <- modnpo_vglm
tmp <- cbind(data, predict(mod, type="response"))

# partial proportioal odds model
mod <- modppo_vglm
tmp <- cbind(data, predict(mod, type="response"))


tmp <- reshape2::melt(tmp,
            id.vars = c("Sex", "Treatment", "Age", "Improved"),
            measure.vars = c("None", "Some", "Marked"),
            variable.name = "Level",
            value.name = "Probability")

head(tmp)


gg <- ggplot(tmp, aes(x = Age, y = Probability, colour = Level)) + 
  geom_line(size = 2.5) + theme_bw() + xlim(10, 80) + 
  geom_point(color = "black", size = 1.5) + facet_grid(Sex ~ Treatment)
gg


# ------------------------------------------------------------------------------
# Effect Plots
# ------------------------------------------------------------------------------
mod <- modpo_polr

plot(Effect("Age", mod))
plot(Effect("Age", mod), style = "stacked", key.args = list(x = 0.55, y = 0.9))

plot(Effect(c("Treatment", "Sex", "Age"), mod), style = "stacked", key.args = list(x = 0.8, y = 0.9))
plot(Effect(c("Treatment", "Age"), mod, latent = TRUE), lwd = 3)


# ------------------------------------------------------------------------------
# 
# deltaMethod is a generic function that uses the delta method to get a first-order approximate standard error
# for a nonlinear function of a vector of random variables with known or estimated covariance matrix.
# ------------------------------------------------------------------------------
source("./utilities/deltaMethod_polr.R")

mod <- modpo_polr

g <- "1 - exp(b20 + b1*x1 + b2*x2 + b3*x3) / ( 1 + exp(b20 + b1*x1 + b2*x2 + b3*x3) )"
g <- "1 - exp(b10 + b1*x1 + b2*x2 + b3*x3) / ( 1 + exp(b10 + b1*x1 + b2*x2 + b3*x3) )"

i <- 100
x1 <- data[i,"Sex"]
x2 <- data[i,"Treatment"]
x3 <- data[i,"Age"]

( calc <- deltaMethod.polr2(object = mod, g = g) )


# ------------------------------------------------------------------------------
# profile LR Confidence interval of each variable's odds ratio
# ------------------------------------------------------------------------------

mod <- modpo_polr
mod <- modnpo_vglm
mod <- modppo_vglm

conf.beta <- confint(object = mod, level = 0.95)
( ci <- exp(conf.beta) )


# ------------------------------------------------------------------------------
# Model Predictive Ability
# ------------------------------------------------------------------------------
library(ROCR)
library(MLmetrics)

mod <- modpo_vglm
mod <- modnpo_vglm
mod <- modppo_vglm

pp <- predict(mod, newdata = data, type = "response")
ll <- as.numeric(data$Improved) * 1 - 1


# histogram of probability by each class
par(mfrow=c(3,1))
pp_0 <- predict(mod, newdata = data %>% filter(Improved == "None"), type = "response")
pp_1 <- predict(mod, newdata = data %>% filter(Improved == "Some"), type = "response")
pp_2 <- predict(mod, newdata = data %>% filter(Improved == "Marked"), type = "response")
hist(pp_0, breaks=seq(0, 1, by = 0.05), include.lowest = T, main = "None", xlab = "", col = "lightblue", ylim = c(0, 30))
hist(pp_1, breaks=seq(0, 1, by = 0.05), include.lowest = T, main = "Some", xlab = "", col = "salmon", ylim = c(0, 30))
hist(pp_2, breaks=seq(0, 1, by = 0.05), include.lowest = T, main = "Marked", xlab = "", col = "lightgreen", ylim = c(0, 30))


# MultiLogLoss
MultiLogLoss(y_pred = pp, y_true = ll)

