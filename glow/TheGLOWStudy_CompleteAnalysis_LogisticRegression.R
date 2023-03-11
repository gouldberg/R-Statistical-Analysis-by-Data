# ------------------------------------------------------------------------------
# The GLOW Study (GLOW500 data)
# Complete Analysis:  Logistic Regression
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "vcdExtra", "effects", "MASS", "VGAM", "car", "broom")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
data <- read.csv("./AppliedLogisticRegression/GLOW/GLOW500.txt", header=T, stringsAsFactors = F, sep="\t")

str(data)

describe(data)

xtabs(~ FRACTURE, data = data)


# Check zero frequency cells  (potential impact to logistic regression's predictive performance)
dat.tab <- xtabs(~ PRIORFRAC + PREMENO + MOMFRAC + ARMASSIST + SMOKE + RATERISK + FRACTURE, data = data)
ftable(dat.tab)

ftable(dat.tab, row.vars = c(1,2))


# Not character, but factor type is required for binreg_plot() and effects packages
data$SUB_ID <- as.character(data$SUB_ID)
data$SITE_ID <- as.character(data$SITE_ID)
data$PHY_ID <- as.character(data$PHY_ID)
data$PRIORFRAC <- as.factor(data$PRIORFRAC)
data$PREMENO <- as.factor(data$PREMENO)
data$MOMFRAC <- as.factor(data$MOMFRAC)
data$ARMASSIST <- as.factor(data$ARMASSIST)
data$SMOKE <- as.factor(data$SMOKE)
data$RATERISK <- as.factor(data$RATERISK)
data$FRACTURE <- as.factor(data$FRACTURE)


# ------------------------------------------------------------------------------
# Overview distribution of each variable and correlation among variables
# ------------------------------------------------------------------------------
library(gpairs)
library(vcd)

var <- c("FRACTURE", "AGE", "WEIGHT", "HEIGHT", "BMI")

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
num_var <- c("AGE", "WEIGHT", "HEIGHT", "BMI")

psych::pairs.panels(data[, c(num_var, "FRACTURE")])


# Margianl plots to check relationship between y and x
op <- par(mfrow(c(2,2), mar = c(4,4,1,2.5) + .1 , cex.lab = 1.4))
col <- c("lightblue", "blue")
par(mfrow=c(2,2))
plot(FRACTURE ~ AGE, data = data, col = col)
plot(FRACTURE ~ WEIGHT, data = data, col = col)
plot(FRACTURE ~ HEIGHT, data = data, col = col)
plot(FRACTURE ~ BMI, data = data, col = col)

# positive relationship with FRACTURE:  HEIGHT
# negative association with FRACTURE:  AGE


# ------------------------------------------------------------------------------
# Association of each categorical and factor variable X against Y
# doubledecker
# ------------------------------------------------------------------------------
catfac_var <- c("PRIORFRAC", "PREMENO", "MOMFRAC", "ARMASSIST", "SMOKE", "SITE_ID", "RATERISK")

# doubledecker
for(i in 1:length(catfac_var)){
  eval(parse(text = paste0("vcd::doubledecker(FRACTURE ~ ", catfac_var[i], ", data = data)")))
}

# positive association with FRACTURE:  PRIORFRAC, MOMFRAC, ARMASSIST, RATERISK
# negative association with FRACTURE:  SMOKE
# no association with FRACTURE:  PREMENO


# ------------------------------------------------------------------------------
# Significant association of each binary variables X against Y
# fourfold display (+ confidence interval of odds ratio)
# ------------------------------------------------------------------------------
bin_var <- c("PRIORFRAC", "PREMENO", "MOMFRAC", "ARMASSIST", "SMOKE")

# fourfold display
for(i in 1:length(bin_var)){
  eval(parse(text = paste0("tmp <- xtabs(~ FRACTURE + ", bin_var[i], ", data = data)")))
  fourfold(tmp, fontsize = 16)
}

# significant association with FRACTURE:  PRIORFRAC, MOMFRAC, ARMASSIST
# no sig with FRACTURE:  PRENOME, SMOKE


# ------------------------------------------------------------------------------
# Significant association of ordinal variables X against Y
# local odds ratio
# ------------------------------------------------------------------------------
confint(loddsratio(FRACTURE ~ RATERISK, data = data, log=TRUE), level = 0.95)
confint(loddsratio(FRACTURE ~ FRACSCORE, data = data, log=TRUE), level = 0.95)

# significant positive association with FRACTURE:  RATERISK(1:2)
# significant negative association with FRACTURE:  FRACSCORE(9:10)


# ------------------------------------------------------------------------------
# Test for the significance of coefficients
# Univariale logistic regression
# ------------------------------------------------------------------------------
source("./utilities/UnivarLogiReg.R")

x_var <- c("PRIORFRAC", "AGE", "WEIGHT", "HEIGHT", "BMI", "PREMENO", "MOMFRAC", "ARMASSIST", "SMOKE"< "RATERISK", "FRACSCORE")
y_var <- "FRACTURE"

crit <- c(0.25, 0.05, 0.001)
uni_lr <- UnivarLogiReg(y_var = y_var, x_var = x_var, data = data, crit = crit)
uni_lr


# Check how bad is this univariate model, compared to a model (saturated model) that fits the data perfectly ?
i <- 1
x_var[i]
eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", x_var[i], ", data = data, family = binomial)")))
vcdExtra::LRstats(mod)


# ------------------------------------------------------------------------------
# Multivariable model with all covariates significant at the 0.25 level in the univariable analysis
# ------------------------------------------------------------------------------
source("./utilities/MultivarLogiReg.R")

# extract only variable with p.val < 0.25
uni_lr
x_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK")

y_var <- "FRACTURE"
crit <- c(0.05, 0.001)

mult_lr <- MultivarLogiReg(y_var = y_var, x_var = x_var, data = data, crit = crit)

mult_lr["mult_lr"]
mult_lr["G.stat"]


# get model object
mod_char <- paste0(x_var, collapse = " + ")
eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
summary(mod)


# ------------------------------------------------------------------------------
# Multivariable model excluding the covariate with the larget p-value that is greater than 0.05
#
# Check to see if removed covariate(s) confound or are needed to adjust the effects of vocariates remaining in the model
# ------------------------------------------------------------------------------
source("./utilities/MultivarLogiReg.R")

# exclude the covariate with the larget p-value that is greater than 0.05 (RATERISK)
mult_lr
x_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST")

y_var <- "FRACTURE"
crit <- c(0.05, 0.001)

mult_lr2 <- MultivarLogiReg(y_var = y_var, x_var = x_var, data = data, crit = crit)

mult_lr2["mult_lr"]
mult_lr2["G.stat"]


# get model object
mod_char <- paste0(x_var, collapse = " + ")
eval(parse(text = paste0("mod2 <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
summary(mod2)


# Compare the model with RATERISK and the model without RATERISK
mult_lr2$G.stat;  mult_lr$G.stat;
mult_lr2$G.stat - mult_lr$G.stat


# nearly significant at the level 0.05
anova(mod2, mod, test="Chisq")


# Compare the Coefficients and check the change in percentage
( coef_mult_lr <- mult_lr$mult_lr %>% dplyr::select(VarName, Coeff) )
( coef_mult_lr2 <- mult_lr2$mult_lr %>% dplyr::select(VarName, Coeff) )
colnames(coef_mult_lr) <- c("CovariateName", "Coeff1")
colnames(coef_mult_lr2) <- c("CovariateName", "Coeff2")
( coef_comp <- coef_mult_lr %>% left_join(., coef_mult_lr2, by="CovariateName") )

( coef_comp <- coef_comp %>% mutate(deltaRatio = round(1 - (Coeff2 / Coeff1), digits = 4)) )

# Largest percentage change is 17% for the coefficient "ARMASSIST".
# This does not exceed our criterion of 20%
# Thus, we see that while self-reported rate of risk (RATERISK) is not a confounder, it is an important covariate.


# ------------------------------------------------------------------------------
# Adding non-significant variable at univariable regression to model and check to see the coefficients become significant
# check the change in p.value
# ------------------------------------------------------------------------------
x_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK")
add_var <- c("WEIGHT", "BMI", "PREMENO", "SMOKE")
y_var <- "FRACTURE"

mod_char_selected <- paste0(x_var, collapse = " + ")

for(i in 1:length(add_var)){
  print(paste0("processing -- ", add_var[i]))
  mod_char <- paste0(mod_char_selected, " + ", add_var[i])
  eval(parse(text = paste0("mod_add <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
  tmp <- tidy(mod_add)
  print(paste0(add_var[i], "  p,value at univariable regression :", round(uni_lr[grep(pattern = add_var[i], uni_lr$VarName), "p.value"], digits = 4)))
  print(paste0(add_var[i], "  p.value at the variable added :", round(tmp[grep(pattern = add_var[i], tmp$term), "p.value"], digits = 4)))
}


# only p.value of BMI is decreased much.


# ------------------------------------------------------------------------------
# Since the coefficients of RATERISK2 is not significant, combine the categories
# ------------------------------------------------------------------------------
source("./utilities/MultivarLogiReg.R")

data <- data %>% mutate(RATERISK_s = ifelse(RATERISK %in% c("1", "2"), "1", ifelse(RATERISK %in% "3", "3", "")))
data$RATERISK_s <- as.factor(data$RATERISK_s)

# exclude the covariate with the larget p-value that is greater than 0.05 (RATERISK)
mult_lr
x_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s")

y_var <- "FRACTURE"
crit <- c(0.05, 0.001)

mult_lr3 <- MultivarLogiReg(y_var = y_var, x_var = x_var, data = data, crit = crit)

mult_lr3["mult_lr"]
mult_lr3["G.stat"]


# get model object
mod_char <- paste0(x_var, collapse = " + ")
eval(parse(text = paste0("mod3 <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
summary(mod3)


# ------------------------------------------------------------------------------
# Main effects + addition of interaction individually
# ------------------------------------------------------------------------------
source("./utilities/MainInterLogiReg.R")

x_main_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s")
x_add_inter_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s")
y_var <- "FRACTURE"

crit <- c(0.10, 0.05, 0.001)

main_inter_lr <- MainInterLogiReg(y_var = y_var, x_main_var = x_main_var, x_add_inter_var = x_add_inter_var, data = data, crit = crit)
main_inter_lr


# ------------------------------------------------------------------------------
# Model with main effects and interaction term
# ------------------------------------------------------------------------------
source("./utilities/MultivarLogiReg.R")

x_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s", "AGE * PRIORFRAC", "MOMFRAC * PRIORFRAC", "MOMFRAC * ARMASSIST")

y_var <- "FRACTURE"
crit <- c(0.05, 0.001)

mult_lr4 <- MultivarLogiReg(y_var = y_var, x_var = x_var, data = data, crit = crit)

mult_lr4["mult_lr"]
mult_lr4["G.stat"]


# Exclude "MOMFRAC * PRIORFRAC" interaction term
x_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s", "AGE * PRIORFRAC", "MOMFRAC * ARMASSIST")
mult_lr5 <- MultivarLogiReg(y_var = y_var, x_var = x_var, data = data, crit = crit)

mult_lr5["mult_lr"]
mult_lr5["G.stat"]


# get model object
mod_char <- paste0(x_var, collapse = " + ")
eval(parse(text = paste0("mod5 <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
summary(mod5)


# Compare main + interaction model vs. only main effects model
# Interaction term contribute to the model.
mult_lr5$G.stat;  mult_lr4$G.stat;  mult_lr3$G.stat;
mult_lr4$G.stat - mult_lr3$G.stat
mult_lr5$G.stat - mult_lr3$G.stat

anova(mod5, mod3, test = "Chisq")


# ------------------------------------------------------------------------------
# Visualize the result of univariable regression model
# and Check the linearlity assumption for continuous variables (AGE and HEIGHT)
# ------------------------------------------------------------------------------
dd <- datadist(data);  options(datadist = "dd");

mod <- glm(FRACTURE ~ AGE, data = data, family = binomial)
mod_lr <- lrm(FRACTURE ~ AGE, data = data)

par(mfrow=c(1,1))

# Logistic regression by only one continuous variables and viualize
# compared to linear regression and lowess curve
plot(jitter((FRACTURE == "1")*1, .1) ~ AGE, data = data, xlim=c(55,95), pch = 16, ylab = "Probability (FRACTURE)")
xvalues <- seq(55, 95, 5)
pred <- predict(mod_lr, newdata = data.frame(AGE = xvalues), type = "response", se.fit = TRUE)
upper <- pred$fit + 1.96 * pred$se.fit
lower <- pred$fit - 1.96 * pred$se.fit
polygon(c(xvalues, rev(xvalues)), c(upper, rev(lower)), col = rgb(0, 0, 1, .2), border = NA)
lines(xvalues, pred$fit, lwd = 4, col = "blue")
abline(mod_lm, lwd = 2)
lines(lowess(data$AGE, data$FRACTURE, f = 0.9), col = "red", lwd = 2)


# Plotting logistic regression by ggplots --> This method does logistic regression and visualization simultaneously !!!
# compared to linear regression and lowess curve
gg <- ggplot(data, aes(x = AGE, y = (FRACTURE == "1")*1)) + xlim(55, 95) + geom_point(position = position_jitter(height = 0.02, width = 0)) +
  stat_smooth(method = "glm", method.args=list(family = "binomial"), alpha = 0.1, fill = "blue", size = 2, fullrange = TRUE)
gg <- gg + stat_smooth(method = "lm", se = FALSE, size = 1.2, color = "black", fullrange = TRUE)
gg <- gg + stat_smooth(method = "lowess", se = FALSE, span = 0.95, colour = "red", size = 1.2)
gg


x_var <- c("AGE", "WEIGHT")
y_var <- "FRACTURE"
i <- 1
par(mfrow=c(1,1))
eval(parse(text = paste0(
"with(data, 
     popbio::logi.hist.plot(independ = ", x_var[i], ", depend = ", y_var, ", 
                            logi.mod = 1,
                            type='hist', counts=TRUE, 
                            boxp = TRUE, rug = TRUE,
                            ylabel = 'Probability (", y_var, ")', xlabel = '", x_var[i], "',
                            col.hist = 'lightblue'))")))


# ------------------------------------------------------------------------------
# Conditioanl Plots
# ------------------------------------------------------------------------------
gg <- ggplot(data, aes(x = WEIGHT, y = (FRACTURE == "1")*1), color = PRIORFRAC) + xlim(30,140) + theme_bw() +
  geom_point(position = position_jitter(height = 0.02, width = 0.2)) +
  stat_smooth(method = "glm", method.args=list(family = "binomial"), alpha = 0.15, size = 2, fullrange = TRUE, aes(fill = PRIORFRAC))
gg

# ggplots can add other stratifying variables by facet_grid()
gg + facet_wrap(~ RATERISK)


# ------------------------------------------------------------------------------
# Check relative contribution and confidence interval of odds ratios for each variables
# ------------------------------------------------------------------------------
dd <- datadist(data);  options(datadist = "dd")

mod <- lrm(FRACTURE ~ ., data = data %>% dplyr::select(-SUB_ID, -SITE_ID, -PHY_ID))

mod

# Check relative contribution
plot(anova(mod))

# Check confidence interval of odds ratios for each variables
plot(summary(mod), log=TRUE, cex = 1.25, col=rgb(.1, .1, .8, alpha=c(.3, .5, .8)))


# ------------------------------------------------------------------------------
# variable selection by stepAIC()
#
#  -->  This selects, often, same model with purposeful selection  -->  Purposeful selection and stepAIC selects same model for this GLOW500 data.
# ------------------------------------------------------------------------------
x_main_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s")
x_add_inter_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s")
x_inter_var <- combinat::combn(x_add_inter_var, m = 2) %>% t() %>% as.data.frame() %>% mutate(x_inter_var = paste0(V1, "*", V2)) %>% dplyr::select(x_inter_var) %>% unlist() %>% unname()

y_var <- "FRACTURE"

mod_main_char <- paste0(x_main_var, collapse = " + ")
mod_inter_char <- paste0(x_inter_var, collapse = " + ")
mod_char <- paste0(mod_main_char, " + ", mod_inter_char)

eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))

step_aic <- stepAIC(mod, trace = TRUE, direction = "both")
step_bic <- stepAIC(mod, trace = TRUE, direction = "both", k = log(nrow(data)))

step_aic$anova
step_bic$anova


# inference for estimated coefficients (Wald test)
lmtest::coeftest(step_aic)
lmtest::coeftest(step_bic)


# Type II tests of each effect
car::Anova(step_aic)
car::Anova(step_bic)


# Likelihood ratio tests of goodness of fit, together with AIC and BIC statistics
vcdExtra::LRstats(step_bic, step_aic)


# Two models are nested. Likelihood ratio test can be applied  --> larger model (step_aic) is significantly better.
anova(step_bic, step_aic, test="Chisq")


# ------------------------------------------------------------------------------
# Variable selection by Fast Backwards Step-Down
#
#  --> singular information matrix for RATERISK_s = 3
# ------------------------------------------------------------------------------
x_main_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s")
x_add_inter_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s")
x_inter_var <- combinat::combn(x_add_inter_var, m = 2) %>% t() %>% as.data.frame() %>% mutate(x_inter_var = paste0(V1, "*", V2)) %>% dplyr::select(x_inter_var) %>% unlist() %>% unname()

y_var <- "FRACTURE"

mod_main_char <- paste0(x_main_var, collapse = " + ")
mod_inter_char <- paste0(x_inter_var, collapse = " + ")
mod_char <- paste0(mod_main_char, " + ", mod_inter_char)

# Fast Backwards Step-Down
eval(parse(text = paste0("modlrm <- lrm(", y_var, " ~ ", mod_char, ", data = data)")))

( fbw <- fastbw(modlrm) )

fbw$names.kept


# ------------------------------------------------------------------------------
# Models
# ------------------------------------------------------------------------------
y_var <- "FRACTURE"

# only main effects:  all variables
x_var <- c("PRIORFRAC", "AGE", "WEIGHT", "HEIGHT", "BMI", "PREMENO", "MOMFRAC", "ARMASSIST", "SMOKE", "RATERISK")
mod_char <- paste0(x_var, collapse = " + ")
eval(parse(text = paste0("mod_mainall <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
eval(parse(text = paste0("modlrm_mainall <- lrm(", y_var, " ~ ", mod_char, ", data = data)")))


# only main effects:  selected
x_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s")
mod_char <- paste0(x_var, collapse = " + ")
eval(parse(text = paste0("mod_mainslc <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
eval(parse(text = paste0("modlrm_mainslc <- lrm(", y_var, " ~ ", mod_char, ", data = data)")))


# main + interaction:  all variables
x_main_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s")
x_add_inter_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s")
x_inter_var <- combinat::combn(x_add_inter_var, m = 2) %>% t() %>% as.data.frame() %>% mutate(x_inter_var = paste0(V1, "*", V2)) %>% dplyr::select(x_inter_var) %>% unlist() %>% unname()

mod_char_main <- paste0(x_main_var, collapse = " + ")
mod_char_inter <- paste0(x_inter_var, collapse = " + ")
mod_char <- paste0(mod_char_main, " + ", x_inter_var)
eval(parse(text = paste0("mod_maininterall <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
# eval(parse(text = paste0("modlrm_maininterall <- lrm(", y_var, " ~ ", mod_char, ", data = data)")))


# final model
x_var <- c("PRIORFRAC", "AGE", "HEIGHT", "MOMFRAC", "ARMASSIST", "RATERISK_s", "AGE * PRIORFRAC", "MOMFRAC * ARMASSIST")
y_var <- "FRACTURE"
mod_char <- paste0(x_var, collapse = " + ")
eval(parse(text = paste0("mod_final <- glm(", y_var, " ~ ", mod_char, ", data = data, family = binomial)")))
eval(parse(text = paste0("modlrm_final <- lrm(", y_var, " ~ ", mod_char, ", data = data)")))


# ------------------------------------------------------------------------------
# Basics of Fitted Model
# Describing the Fitted Model
# ------------------------------------------------------------------------------
mod <- mod_final
modlrm <- modlrm_final


# Hypothesis tests for regression parameters (type1 sequential test)
# Hypothesis tests for regression parameters (type2 partial test)
anova(mod, test="Chisq")
Anova(mod, test="LR")


# relative contribution based on Wald statistics
plot(anova(modlrm))


# Odds Ratio:  Estimated
exp(coef(mod))


# Odds Ratio:  Wald confidence interval
exp(confint.default(mod, level = 0.95))
summary(modlrm)
plot(summary(modlrm), log=TRUE, cex = 1.25, col=rgb(.1, .1, .8, alpha=c(.3, .5, .8)))


# Odds Ratio:  Profile likelihod ratio interval
exp(confint(mod, level = 0.95))


b <- bootcov(f, B = 1000)

# Nomogram:  logodds
# Nomogram:  logodds and probability
par(mfrow=c(1,1))
plot(nomogram(modlrm))
plot(nomogram(modlrm, fun=plogis, funlabel="Probability", fun.at=c(.01, .05, .1, .25, .5, .75, .9, .95, .99)))


# ------------------------------------------------------------------------------
# Model Performance
# Discrimination Indexes
# ------------------------------------------------------------------------------
round(modlrm_mainall$stats, digits = 4)
round(modlrm_mainslc$stats, digits = 4)
round(modlrm_final$stats, digits = 4)


# ------------------------------------------------------------------------------
# Model Predictive Ability
# ROC curve,  Precision-Recall curve,  Sensitivity vs Specificity,  Lift charts
# ------------------------------------------------------------------------------
library(ROCR)
library(MLmetrics)

mod <- mod_final

pp <- predict(mod, newdata = data, type = "response")
ll <- as.numeric(data$FRACTURE) * 1 - 1


# histogram of probability by each class
par(mfrow=c(2,1))
pp_ps <- predict(mod, newdata = data %>% filter(FRACTURE == "1"), type = "response")
pp_ng <- predict(mod, newdata = data %>% filter(FRACTURE == "0"), type = "response")
hist(pp_ps, breaks=seq(0, 1, by = 0.05), include.lowest = T, main = "Positive Class", xlab = "", col = "lightblue", ylim = c(0, 70))
hist(pp_ng, breaks=seq(0, 1, by = 0.05), include.lowest = T, main = "Negative Class", xlab = "", col = "salmon", ylim = c(0, 70))


# confusion matrix
caret::confusionMatrix(data = (pp < 0.5)*1, reference = ll, positive = "1")


# data Conversion
pred <- prediction(predictions = pp, labels = ll)


# ROC curve, Precision-Recall curve,  Sensitivity vs Specificity,  Lift chart
perf_roc <- performance(pred, "tpr", "fpr")
perf_pr <- performance(pred, "prec", "rec")
perf_ss <- performance(pred, "sens", "spec")
perf_lift <- performance(pred, "lift", "rpp")


# plotting
par(mfrow=c(2,2))
plot(perf_roc, avg= "threshold", colorize=T, lwd= 3, main= "... ROC curve ...")
plot(perf_roc, lty=3, col="grey78", add=T)
plot(perf_pr, avg= "threshold", colorize=T, lwd= 3,main= "... Precision/Recall curve ...")
plot(perf_pr, lty=3, col="grey78", add=T)
plot(perf_ss, avg= "threshold", colorize=T, lwd= 3, main="... Sensitivity/Specificity plot ...")
plot(perf_ss, lty=3, col="grey78", add=T)
plot(perf_lift, avg= "threshold", colorize=T, lwd= 3, main= "... Lift chart ...")
plot(perf_lift, lty=3, col="grey78", add=T)


# AUC, F1, Gini
performance(pred, measure = "auc")@y.values[[1]]
F1_Score(y_pred = (pp < 0.5)*1, y_true = ll, positive = "1")
Gini(y_pred = pp, y_true = ll)
LogLoss(y_pred = pp, y_true = ll)


# ROC curve: variaout type
par(mfrow=c(2,2))
plot(perf_roc, colorize=T, lwd=2, main='ROC curves from 10-fold cross-validation')
plot(perf_roc, avg='vertical', spread.estimate='stderror',lwd=3,main='Vertical averaging + 1 standard error',col='blue')
plot(perf_roc, avg='horizontal', spread.estimate='boxplot',lwd=3,main='Horizontal averaging + boxplots',col='blue')
plot(perf_roc, avg='threshold', spread.estimate='stddev',lwd=2, main='Threshold averaging + 1 standard deviation',colorize=T)


# ------------------------------------------------------------------------------
# Full-model plots
#
# Disply of fitted values for a binary regression model with one numeric predictor, conditioned by zero or many co-factors
# Check baseline (no risk factors) and risk factors impact
# ------------------------------------------------------------------------------
mod <- mod_final

# Fitted log odds of FRACTURE
# by default, binreq_plot() uses the first numeric predictors as the horizontal variable
vcd::binreg_plot(mod, type = "link", conf_level = 0.68,
            legend = FALSE, labels = TRUE, labels_just = c("right", "bottom"),
            cex = 0, point_size = 0.8, pch = 15:17,
            ylab = "Log odds (FRACTURE)",  ylim = c(-7, 4))

# Fitted probaility of FRACTURE
vcd::binreg_plot(mod, type = "response", conf_level = 0.68,
                 legend = FALSE, labels = TRUE, labels_just = c("right", "bottom"),
                 cex = 0, point_size = 0.8, pch = 15:17,
                 ylab = "Probability (FRACTURE)",  ylim = c(0, 1))

# subsetting
vcd::binreg_plot(mod, type = "link", subset = admit == "Emerg", main = "Emerg",
                 conf_level = 0.68,
                 legend = FALSE, labels = TRUE, labels_just = c("right", "bottom"),
                 cex = 0, point_size = 0.8, pch = 15:17,
                 ylab = "Log odds (died)",  ylim = c(-7, 4))


# ------------------------------------------------------------------------------
# Effect plots
#  -- and partial residual plots --> checking indication of nonlinearlity
# ------------------------------------------------------------------------------
mod <- mod_final
modlrm <- modlrm_final


# Main effect (default is response probabilities)
eff <- effects::allEffects(mod, partial.residuals = TRUE)
names(eff)

eff[["HEIGHT"]]
eff[["HEIGHT"]]$model.matrix %>% head()


# plot main effets of each variable (default is reponse probabilities)
plot(eff)


# plot main effects and partial residuals for numeric variables with smoothed loess curve
plot(eff, rows = 2, cols = 2, type = "response", residuals.pch = 15)


# effect plots for several predictors jointly or full-model plots
eff2 <- Effect(c("AGE", "PRIORFRAC"), mod)
eff2 <- Effect(c("MOMFRAC", "ARMASSIST"), mod)

plot(eff2, 
     mutiline = TRUE, ci.stype = "bands",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)),
     key.args = list(x = 0.52, y = 0.92, columns = 1), grid = TRUE)

plot(eff2, 
     mutiline = TRUE, ci.stype = "bands", type = "response",
     colors = c("red", "blue"), lwd = 3,
     ticks = list(at = c(.05, .1, .25, .5, .75, .9, .95)),
     key.args = list(x = 0.52, y = 0.92, columns = 1), grid = TRUE)


# Partial Effects (log odds scale)
ggplot(Predict(modlrm), sepdiscrete="vertical", vnames="names", rdata=data, histSpike.opts=list(frac=function(a) .1 * a/max(a)))


# ------------------------------------------------------------------------------
# Goodness of Fit Test
# ------------------------------------------------------------------------------
mod <- mod_final

# --------------------------------------
# Hosmer - Lemeshow Test --> mod_final: p = 0.6034 indicates no evidence of poor fit
# but only main effects model p = 0.1522
library(ResourceSelection)

( hl <- hoslem.test(mod$y, fitted(mod), g=10) )
hoslem.test(mod_mainall$y, fitted(mod_mainall), g=10)
hoslem.test(mod_mainslc$y, fitted(mod_mainslc), g=10)
hoslem.test(mod_maininterall$y, fitted(mod_maininterall), g=10)

# Observed vs Expected
cbind(hl$observed, hl$expected)


# --------------------------------------
# Squared Pearson Correlation Coefficients of observed outcome with the estimated probability
# mod_final has largest value
mod <- mod_mainall
mod <- mod_mainslc
mod <- mod_maininterall
mod <- mod_final

pred <- predict(mod, newdata = data, type = "response")
cor((data$FRACTURE == "1")*1, pred)^2



# ------------------------------------------------------------------------------
# DIGNOSTICS:
# Component-plus-residual plots (partial residual plot)  -->  This plot is not available for the model with interaction term
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# DIAGNOSTICS:
# residuals
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Studentized Residuals  --> can also check influential observations
# and Standardized Pearson Residuals
stud.resid <- rstudent(mod)
stand.resid <- rstandard(model = mod, type = "pearson")

par(mfrow=c(2,1))
plot(stud.resid, ylim = c(min(-3, stud.resid), max(3, stud.resid)), main = "Studentized residuals", type = "l")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))
plot(stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Pearson residuals", type = "l")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

par(mfrow=c(2,1))
qqnorm(stud.resid);  qqline(stud.resid);
qqnorm(stand.resid);  qqline(stand.resid);


# --------------------------------------
# Standardized Pearson Residuals vs. explanatory variable
stand.resid <- rstandard(model = mod, type = "pearson")
par(mfrow=c(1,1))
plot(x = data$age, y = stand.resid, ylim = c(min(-3, stand.resid), max(3, stand.resid)), main = "Standardized Pearson residuals")
abline(h = c(3,2,0,-2,-3), lty = "dotted", col = c("red", "blue", "gray", "blue", "red"))

smooth.stand <- loess(stand.resid ~ age, data = data)
ord <- order(data$age)
lines(x = data$age[ord], y = predict(smooth.stand)[ord], lty = "solid", col = "red")



# ------------------------------------------------------------------------------
# DIGNOSTICS plot (default)
# ------------------------------------------------------------------------------
par(mfrow=c(3,2))
plot(mod, which = 1)
plot(mod, which = 2)
plot(mod, which = 3)
plot(mod, which = 4)
plot(mod, which = 5)
plot(mod, which = 6)


# ------------------------------------------------------------------------------
# DIGNOSTICS:
# leverage (hatvalues)
# ------------------------------------------------------------------------------

# model's number of coefs including intercept
k <- 5
( crit1 <- 2 * k / nrow(data) )
( crit2 <- 3 * k / nrow(data) )

ht <- hatvalues(mod)
lv <- ht / (1 - ht)


# hatvalue and leverage
par(mfrow=c(2,1))
plot(ht, type="h", main = "hatvalue")
abline(h = c(crit1, crit2), lty=c(2,2), col=c("orange", "red"))
plot(lv, type="h", main = "Leverage = h / (1 - h)")


# ------------------------------------------------------------------------------
# DIGNOSTICS:
# hatvalue, approx change in Pearson Chi^2, approx change in deviance, and approx Cook's Distance  VS. estimated mean or probability
# hatvalue (leverage)  VS. approx change in Pearson Chi^2, approx change in deviance, and approx Cook's Distance
# ------------------------------------------------------------------------------
source("./utilities/glmDiagnostics.R")

save.diag <- glmInflDiag(mod.fit = mod, print.output = TRUE, which.plots = c(1,2))

# check potentially influential data
round(head(save.diag, n = 3), digits = 2)


# ------------------------------------------------------------------------------
# DIGNOSTICS:
# Influence (Cook's Distance) VS. Leverage (h / (1 - h))
# ------------------------------------------------------------------------------

# Cook's distance vs Leverage (h / (1 - h))
par(mfrow=c(3,1))
plot(mod, which = 4)
abline(h = c(4/nrow(data), 1), lty=2, col=c("orange","red"))
plot(hatvalues(mod)/(1 - hatvalues(mod)), type="h", xlab="Index", ylab="Leverage", main = "Leverage h/(1-h)")
plot(mod, which = 6)


# Also ..
save.diag <- glmInflDiag(mod.fit = mod, print.output = TRUE, which.plots = c(2))


# ------------------------------------------------------------------------------
# DIGNOSTICS:
# Leverage (hatvalue) VS. Studentized Pearson Residuals + Cook's Distance
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(mod, which = 5)

par(mfrow=c(1,1))
( res <- car::influencePlot(mod, id.col = "red", scale = 9, id.cex = 1.5, id.n = 5) )

idx <- which(rownames(data) %in% rownames(res))
var <- c("age", "cancer", "admit", "uncons")
cbind(data[idx, var], res)


# Influence Index Plot
car::influenceIndexPlot(mod, vars = c("Cook", "Studentized", "hat", id.n = 4))


# ------------------------------------------------------------------------------
# DIGNOSTICS:
# Computes some of the regression (leave-one-out deletion) diagnostics
# DEBETAS, DFFITS, cov.r, Cook's Distance, Hatvalue
# ------------------------------------------------------------------------------
( infl <- influence.measures(mod) )

# obeervations considered noteworthy on one or more of these statistics as indicated by a "*"
summary(infl)


dfbetas <- data.frame(infl$infmat[,2:9])
op <- par(mar = c(5,5,1,1) + .1)
cols <- ifelse(data$FRACTURE == "1", "red", "blue")

crit <- 0.25

plot(dfbetas[,1], type = "h", col = cols, xlab = "Observation index", ylab = "expression(Delta * beta[])", cex.lab = 1.3)
points(dfbetas[,1], col = cols)
big <- abs(dfbetas[,1]) > crit
idx <- 1:nrow(dfbetas)
text(idx[big], dfbetas[big,1], label=rownames(dfbetas)[big], cex=0.9, pos=ifelse(dfbetas[big,1] > 0, 3, 1), xpd = TRUE)
abline(h = c(-crit, 0, crit), col = "gray")
par(op)


# ------------------------------------------------------------------------------
# show the pairwise changes in the regression coefs for the various predictors
# THe DFBETAs for cancer, admit, and uncons are all extremely peaked, yet the pairwise plots show considerable structure
car::scatterplotMatrix(dfbetas, smooth=FALSE, id.n = 2, ellipse = TRUE, levels = 0.95, robust = FALSE, diagonal = "histogram", groups = data$FRACTURE, col = c("blue","red"))


# ------------------------------------------------------------------------------
# DIGNOSTICS
# Added-Variable Plots (partial regression plots)
# ------------------------------------------------------------------------------
pch <- ifelse(data$FRACTURE == "1", 1, 2)
car::avPlots(mod, id.n = 2, pch=pch, cex.lab=1.3)


# Now add "systolic", which was nominated by several different procedures
mod_add <- glm(died ~ age + cancer + admit + uncons + systolic, data = data, family = binomial)
anova(mod, mod_add, test = "Chisq")


# Added-variable plot for the effect of adding systolic blood pressure to the main effects model
# In this plot, cases 331 and 921 have high partial leverage, but they are not influential.
# Case 84, however, has high leverage and a large residual, so it is possibly influential on the evidence for inclusion of systolic in the model
par(mfrow=c(1,1))
car::avPlots(mod_add, "systolic", id.n=3, pch = pch)



# ------------------------------------------------------------------------------
# Study Uncertainty of the model:  Validating the Fitted Model
# ------------------------------------------------------------------------------
modlrm <- update(modlrm_mainall, x=TRUE, y=TRUE)
modlrm <- update(modlrm_mainslc, x=TRUE, y=TRUE)
modlrm <- update(modlrm_final, x=TRUE, y=TRUE)


# check Optimistic level
( s <- modlrm$stats )
( gamma.hat <- (s["Model L.R."] - s["d.f."]) / s["Model L.R."] )
( 1 - gamma.hat )


# ------------------------------------------
# Check model performance optimism level by Bootstrapping
B = 200
v <- validate(modlrm, B=B, bw=TRUE, pr=TRUE)

# check the optimism and corrected for each statistics such as Dxy
v[1:11,]

# show first X bootstrap resamples
plot(v)


# ------------------------------------------
# Compute bootstrap estimates of the log odds ratio
B = 10000
b <- bootcov(modlrm, B=B)

summary(b)


# distribution of bootstrap coefficient
hist(b$boot.Coef[,"uncons=Yes"], lty = 1, nclass = 100, xlab = "log(OR)")
sd(b$boot.Coef[,"uncons=Yes"])


# bootstrap confidence interval of coefficient
quantile(b$boot.Coef[,"uncons=Yes"], c(0.025, 0.5, 0.975))

