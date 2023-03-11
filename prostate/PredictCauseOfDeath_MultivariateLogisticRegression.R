# ------------------------------------------------------------------------------
# Predict Cause of Death
# Multivariate Logistic Regression Overview
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
getHdata(prostate)

# convert an old data format to R format
prostate$sdate <- as.Date(prostate$sdate)
data <- prostate

str(data)

describe(data)

# scatter plot matrix
psych::pairs.panels(data)


# check is.factor
v_fac <- sapply(1:ncol(data), function(x) is.factor(data[,x]))
v_fac <- colnames(data)[v_fac]


# check sum(is.na)
sum(is.na(data))
check_sumisna <- function(data){ sum(is.na(data)) }
apply(data, FUN=check_sumisna, MARGIN=2)


# corrplot
# correlation is very small 
corrplot(cor(data[, !colnames(data) %in% v_fac])) 


# correlation by rank  -->  need to exclude NA rows
data_s <- dplyr::select(data, -status, -dtime, -sdate, -patno)
( corr <- cor(apply(data[complete.cases(data_s),], FUN=rank, MARGIN=2)) )
corrplot(corr, order="hclust", addrect=4)


#
boxplot(patno ~ status, data)


# ------------------------------------------------------------------------------
# Data transformation and Single Imputation
# ------------------------------------------------------------------------------
data <- within(data, {
    levels(ekg)[levels(ekg) %in% c("old MI", "recent MI")] <- "MI"
    ekg.norm = 1 * (ekg %in% c("normal", "benign"))
    levels(ekg) <- abbreviate(levels(ekg))
    pfn <- as.numeric(pf)
    levels(pf) <- levels(pf)[c(1,2,3,3)]
    cvd <- status %in% c("dead - heart or vascular", "dead - cerebrovascular")
    rxn = as.numeric(rx)
})


str(data)
xtabs(~ cvd, data = data)

# 
ptrans <- transcan(~ sz + sg + ap + sbp + dbp + age + wt + hg + ekg + pf + bm + hx + dtime + rx,
                   imputed = TRUE, transformed = TRUE, data = data, trantab = TRUE, pl = FALSE, pr = FALSE)

summary(ptrans, digits=4)

# imputed value and transformed value
ptrans$imputed
ptrans$transformed

# check Coefficients of canonical variates for predicting each (row) variable
# sbp is predicted almost soley from dbp and bm is predicted mainly from ap, hg, and pf
round(ptrans$xcoef, digits=4)

# plot original value and transformed value by each variable
# Imputed values are show as red plus signs
# Transformed values are arbitrarily scaled to [0,1]
# NOTE:  ap requires additional log transformation
ggplot(ptrans, scale=TRUE) + theme(axis.text.x = element_text(size=6))

# imputation
imp <- impute(ptrans, data=data, list.out=TRUE)
NAvars <- all.vars(~ sz + sg + age + wt + ekg)
for(x in NAvars) data[[x]] <- imp[[x]]

# extract data
subset <- data$status %in% c("dead - heart or vascular", "dead - cerebrovascular", "dead - prostatic ca")
trans <- ptrans$transformed[subset,]
psub <- data[subset,]


# ------------------------------------------------------------------------------
# Estimate how many PCs to be used ?
# ------------------------------------------------------------------------------
# As Rule of Thumbs 15:1  --> 8 PCs to be used
k <- 15
nrow(data %>% filter(status %in% c("dead - heart or vascular", "dead - cerebrovascular"))) / k

n_pc <- 8


# ------------------------------------------------------------------------------
# Regression on Origianl Variables, PCs and Pretransformations
# ------------------------------------------------------------------------------
# FUNCTION: compute the first k PCs
ipc <- function(x, k=1, ...) prcomp(x, ..., scale=T)$x[,1:k]

# Extract first 8 PC score of raw and transformed data
pc8 <- ipc(~ sz + sg + log(ap) + sbp + dbp + age + wt + hg + ekg.norm + pfn + bm + hx + rxn + dtime, data = psub, k = n_pc)
pc8t <- ipc(trans, k = n_pc)

# Regression
# cvd in psub data, but pc8 and pc8t not in psub data, but it works in this way.
f8 <- lrm(cvd ~ pc8, data = psub)
f8t <- lrm(cvd ~ pc8t, data = psub)
f <- lrm(cvd ~ sz + sg + log(ap) + sbp + dbp + age + wt + hg + ekg + pf + bm + hx + rx + dtime, data = psub)
g <- lrm(cvd ~ rcs(sz,4) + rcs(sg,4) + rcs(log(ap),4) + rcs(sbp,4) + rcs(dbp,4) + rcs(age,4) + rcs(wt,4) + rcs(hg,4) + ekg + pf + bm + hx + rx + rcs(dtime,4), data = psub)
h <- lrm(cvd ~ trans, data=psub)


# Best in AIC is f8t model
# Based on AIC, the more traditional model fitted to the raw data and assuming linearity for all the continuous predictions has only a slight chance of
# producing worse cross-validated predictive accuracy than other methods
AIC(f8); AIC(f8t); AIC(f); AIC(g); AIC(h)


# ------------------------------------------------------------------------------
# Description of Fitted Model
#
# Central Tendency of prediction errors
#  -- Breir score:  sum(P-hat - Y)^2 / n
#                   the quadratic proper scoring rule that combines calibration adn discrimination.
#                   The cross-validated error rate corrects the apparent error rate only if the predicted probability is exactly 1/2 or is 1/2 +- 1/(2n)
#                   Breir score does not have this problem and have the nice property of being maximized when the predicted probabilities are the population probabilities.
#
# Discrimination Measures
#  -- Kendall's tau
#  -- Sommers' Dxy:  rank correlation of Y-hat and Y.
#                    When Y is binary, Dxy = 2 * (c - 0.5) where c is the concordance probability or AU
#                    = the difference between concordance and discordance probabilities
#                    0:  mode is making random predictions   1: predictions are perfectly discriminating
#                    In general, rank-based indexes have advantage of being insensitive to the prevalence of positive responses.
#
# Discrimination measures based on variation in Y-hat
#  -- g-Index:  the mean over all possible i <> j of |Zi - Zj, which is based on Gini's mean difference for a variable Z
#               interpretable, robust, and highly efficient measure of variation.
#  -- gp:       probability version of g-Index, Gini's mean difference of P-hat.
#               represent e.g. "typical" odds ratios, and "typical" risk differences.

# gp:  Gini's mean difference of P (in logistic regression)

# ------------------------------------------------------------------------------
# select model
mod <- f

# Summary statistics
print(mod)
mod$stats

# Wald-ANOVA table
an <- anova(mod)
print(an)


# plot relative contribution of each factor in the model
# Ranking of apparent importance of predictors
plot(an)


# CHECK Optimistic level
# van Houwelingen-Le Cessie heuristic shrinkage estimation
# "1 - gamma.hat":  this model will validate on new data about X% worse than on this dataset.
# this can be shown to equal = { (n - p - 1) / (n - 1) } * { R^2adjusted / R^2 }
( s <- mod$stats )
( gamma.hat <- (s["Model L.R."] - s["d.f."]) / s["Model L.R."] )
( 1 - gamma.hat )


# Partial Effects (log odds scale)
dd <- datadist(psub);  options(datadist="dd")
ggplot(Predict(mod), sepdiscrete="vertical", vnames="names", rdata=psub, histSpike.opts=list(frac=function(a) .1 * a/max(a)))


# Odds Ratios
# interquartile-range odds ratios for continuout predictors and simple odds ratios for categorical predictors
# the bars represent 0.9, 0.95, 0.99
# numbers at left are upper quartile and lower quartile, or current group
plot(summary(mod), log=TRUE)



# ------------------------------------------------------------------------------
# Backwards Step-Down
#
# Identify the variables that explain the bulk of the cause of death.
# The greatly reduced model results in a simple nomogram
#
# fastbw uses AIC as a stopping rules or p to use P-values
# AIC case: a factor is deleted if the chi-square falls below twice its defrees of freedom
# ------------------------------------------------------------------------------
# select model
mod <- f

# Fast Backwards Step-Down
( fbw <- fastbw(mod) )

fbw$names.kept


# final model
mod_fin <- lrm(cvd ~ sz + log(ap) + age + hx, data = psub)
mod_fin

plot(anova(mod_fin))

plot(summary(mod_fin))


# ------------------------------------------------------------------------------
# Compare Models
# ------------------------------------------------------------------------------
AIC(mod);  AIC(mod_fin)
mod$stats;  mod_fin$stats

round(1 - (mod$stats["Model L.R."] - mod$stats["d.f."]) / mod$stats["Model L.R."], digits=4)
round(1 - (mod_fin$stats["Model L.R."] - mod_fin$stats["d.f."]) / mod_fin$stats["Model L.R."], digits=4)


# ------------------------------------------------------------------------------
# Nomogram for final model
#
# For each predictor, read the points assigned on the 0-100 scale and add these points.
# Read the result on the Total Points scale and the read the corresponding predictors below it.
# ------------------------------------------------------------------------------
nom <- nomogram(mod_fin, ap=c(.1, .5, 1, 5, 10, 50), fun=plogis, funlabel="Probability", fun.at=c(.01, .05, .1, .25, .5, .75, .9, .95, .99))
plot(nom, xfrac = .45)


# ------------------------------------------------------------------------------
# Study Uncertainty of the model by bootstrapping
#
# Use the bootstrap to study the uncertainty in the selection of variables and
# to penalize for this uncertainty when estimating predictive performance of the model
#
# Dxy: Somer's rank correlation between predicted probability that Y=1 vs the binary Y values
# D: Discrimination indx.  likelihood ratio chi^2 divided by the sample size = (model L.R chi^2 - 1) / n
# U: Unreliability index.  unitless index of how far the logit calibration curve intercept and slope are from (0,1)
#                          = (difference in -2 log likelihood between uncalibrated Y and Y with overall intercept and slop calibrated to test sample) /  n
# Q: Logarithmic accuracy score.  a scaled version of the log-likelihood achieved by the predictive model = D - U
# Intercept:  Calibration intercept on logit scale
# Slope:  Calibration slope (slope of predicted log odds vs true log odds)
# ------------------------------------------------------------------------------
# select model
mod <- f

mod <- update(mod, x=TRUE, y=TRUE)


# Bootstrapping the model
B = 200
v <- validate(mod, B=B, bw=TRUE, pr=TRUE)


# check the optimism and corrected for each statistics such as Dxy
v


# show first X bootstrap resamples
plot(v)


# a nearly unbiased estimate of future calibration of the stepwise derived model
# rug plot showing the distribution of predicted risks
# the smooth nonparametric calibration estimator (loess) is used.
cal <- calibrate(mod, B=B, bw=TRUE, pr=TRUE)

mod_fin <- update(mod_fin, x=TRUE, y=TRUE)
cal_fin <- calibrate(mod_fin, B=B, bw=TRUE, pr=TRUE)

# compare with final model
print(cal, latex=TRUE)
print(cal_fin, latex=TRUE)

plot(cal)
plot(cal_fin)


# --> less optimism but smaller Dxy due to loss of information from removing moderately important variables...
# Shrinkage + full model, it would have better calibration and discrimination than reduced model ?


# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------
phat <- predict(mod, data = psub, type="fitted")
phat_fin <- predict(mod_fin, data = psub, type="fitted")


# ------------------------------------------------------------------------------
# Use better significance level for variable to stay in the model
# Use individual approximate Wald tests rather than tests combining all deleted variables
# ------------------------------------------------------------------------------
B <- 200
v5 <- validate(mod, bw=TRUE, sls=0.5, type="individual", B=B, pr=T)


# optimism is smaller
v5


