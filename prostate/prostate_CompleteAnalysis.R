# ------------------------------------------------------------------------------
# Predict cause of death (prostate data)
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "vcdExtra", "effects", "MASS", "VGAM", "car", "broom", "psych")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

rm(list=ls())


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
getHdata(prostate)

# convert an old data format to R format
prostate$sdate <- as.Date(prostate$sdate)
data <- prostate
str(data)


Hmisc::describe(data)

# repsonse variable
Hmisc::describe(data$status)
table(data$status)

data$cvd <- data$status %in% c("dead - heart or vascular", "dead - cerebrovascular")


# combining an infrequent category with the next category, and dichotomizing ekg
levels(data$ekg)[levels(data$ekg) %in% c("old MI", "recent MI")] <- "MI"
data$ekg.norm <- 1 * (data$ekg %in% c("normal", "benign"))
levels(data$ekg) <- abbreviate(levels(data$ekg))
data$pfn <- as.numeric(data$pf)
levels(data$pf) <- levels(data$pf)[c(1,2,3,3)]
data$rxn = as.numeric(data$rx)


# check is.factor
var_fac <- sapply(1:ncol(data), function(x) is.factor(data[,x]))
( var_fac <- colnames(data)[var_fac] )

var_char <- sapply(1:ncol(data), function(x) is.character(data[,x]))
( var_char <- colnames(data)[var_char] )

var_num <- sapply(1:ncol(data), function(x) is.numeric(data[,x]))
( var_num <- colnames(data)[var_num] )

var_int <- sapply(1:ncol(data), function(x) is.integer(data[,x]))
( var_int <- colnames(data)[var_int] )

var_bin <- c("stage", "bm", "hx", "ekg.norm")

var_cat <- c("sg", "dbp", "sbp", "rx", "pf", "status", "ekg")


# check sum(is.na)
sum(is.na(data))
check_sumisna <- function(data){ sum(is.na(data)) }
apply(data, FUN=check_sumisna, MARGIN=2)


# Check zero frequency cells  (potential impact to logistic regression's predictive performance)
dat.tab <- xtabs(~ rx + pf + cvd, data = data)
ftable(dat.tab)

dat.tab <- xtabs(~ stage + bm + hx + cvd, data = data)
ftable(dat.tab)

ftable(dat.tab, row.vars = c(1,2))


# ------------------------------------------------------------------------------
# Overview distribution of each variable (numeric, integer) and correlation among variables
# ------------------------------------------------------------------------------
library(gpairs)
library(vcd)

( var <- setdiff(unique(var_num, var_int), var_bin) )

# marical and conditional plots
gpairs(data[,var],
       diag.pars = list(fontsize = 16, hist.color ="lightgray",
                        mosaic.pars = list(gp = shading_Friendly, gp_args = list(interpolate=1:4)),
                        outer.rot = c(45,45)))


# check correlation
psych::pairs.panels(data[, var])


# corrplot
# correlation is very small 
corrplot(cor(data[, colnames(data) %in% var])) 


# ------------------------------------------------------------------------------
# The relationship of numeric variables X against Y
# Margianl plots
# ------------------------------------------------------------------------------
( var <- setdiff(unique(var_num, var_int), var_bin) )

psych::pairs.panels(data[, c(var, "cvd")], bg=c("blue", "yellow")[data$cvd])


# Margianl plots to check relationship between y and x
op <- par(mfrow=c(2,2), mar = c(4,4,1,2.5) + .1 , cex.lab = 1.4)
col <- c("lightblue", "blue")
par(mfrow=c(2,2))
plot(cvd ~ age, data = data, col = col)
plot(cvd ~ wt, data = data, col = col)
plot(cvd ~ sbp, data = data, col = col)
plot(cvd ~ dbp, data = data, col = col)
plot(cvd ~ hg, data = data, col = col)
plot(cvd ~ sz, data = data, col = col)
plot(cvd ~ sg, data = data, col = col)
plot(cvd ~ ap, data = data, col = col)



# ------------------------------------------------------------------------------
# Association of each categorical and factor variable X against Y
# doubledecker
# ------------------------------------------------------------------------------
var <- unique(c(var_fac, var_cat, var_bin))

# doubledecker
for(i in 1:length(var)){
  eval(parse(text = paste0("vcd::doubledecker(cvd ~ ", var[i], ", data = data)")))
}


# ------------------------------------------------------------------------------
# Significant association of each binary variables X against Y
# fourfold display (+ confidence interval of odds ratio)
# ------------------------------------------------------------------------------
var <- var_bin

# fourfold display
for(i in 1:length(var)){
  eval(parse(text = paste0("tmp <- xtabs(~ cvd + ", var[i], ", data = data)")))
  fourfold(tmp, fontsize = 16)
}

# significant association with cvd:  hx, stage
# no sig with FRACTURE:  bm


# ------------------------------------------------------------------------------
# Significant association of ordinal variables X against Y
# local odds ratio
# ------------------------------------------------------------------------------
confint(loddsratio(cvd ~ sbp, data = data, log=TRUE), level = 0.95)
confint(loddsratio(cvd ~ dbp, data = data, log=TRUE), level = 0.95)
confint(loddsratio(cvd ~ sg, data = data, log=TRUE), level = 0.95)



# ------------------------------------------------------------------------------
# Redundancy Analysis
# ------------------------------------------------------------------------------
r2_cutoff <- 0.3
redun(~ stage + I(rxn) + age + wt + I(pfn) + hx + sbp + dbp + ekg.norm + hg + sz + sg + ap + bm, r2 = r2_cutoff, type="adjusted", data = data)

# redundant variables:  stage, sbp, bm, sg
# stage can be predicted with R^2 = 0.658 (0.655) from other 13 variables,
# but only with R^2 = 0.493 (0.488) after deletion of 3 variables lated declared to be redundant.


# ------------------------------------------------------------------------------
# Variable Clustering
#
# General and robust similarity measure is Hoeffding's D
# D detects nonmonotonic associations
# ------------------------------------------------------------------------------
# data has list type, so do not use dplyr::select()
x <- with(data, cbind(stage, rx, age, wt, pf, hx, sbp, dbp, ekg.norm, hg, sz, sg, ap, bm))
str(x)


# spearman rho rank correlation plot
r <- rcorr(x, type="spearman")$r
maxabsr <- max(abs(r[row(r) != col(r)]))
p <- nrow(r)
v <- dimnames(r)[[1]]
plot(c(-.35, p+.5), c(.5, p+.25), type="n", axes=FALSE, xlab="", ylab="")
text(rep(.5, p), 1:p, v, adj=1)
for(i in 1:(p-1)){
  for(j in (i+1):p){
    lines(c(i,i), c(j,j+r[i,j]/maxabsr/2), lwd=3, lend="butt")
    lines(c(i-.2, i+.2), c(j,j), lwd=1, col=grey(.7))
  }
  text(i, i, v[i], str=-45,  adj=0)
}


# Variable Clustering  -->  we combine sbp and dbp, and tentatively combine ap, sg
vc <- varclus(~ stage + rxn + age + wt + pfn + hx + sbp + dbp + ekg.norm + hg + sz + sg + ap + bm, sim="hoeffding", data=data)
plot(vc)


# ------------------------------------------------------------------------------
# Single imputations (before regression analysis) and transformation
# ------------------------------------------------------------------------------
ptrans <- transcan(~ sz + sg + ap + sbp + dbp + age + wt + hg + ekg + pf + bm + hx + dtime + rx, data = data,
                   imputed = TRUE, transformed = TRUE, trantab = TRUE, show.na = TRUE, frac = 0.1, pl = TRUE, pr = TRUE)


# 
ggplot(ptrans, scale=TRUE) + theme(axis.text.x = element_text(size=6))


# age,, wt, ekg, pf, hex are not strongly related to other variables
# --> Imputations for those are relying more on the median or modal values from the marginal distribution
# From the coeffs of first (standardized) canonical variates, sbp is predicted almost solely from dbp,
# bm is predicted mainly from ap, hg and pf
summary(ptrans, digits = 4)


# the value to be imputed
ptrans$imputed


# impute all missing values in all variables given to transcan
imp <- impute(ptrans, data = data, list.out=TRUE)

var_na <- c("sz", "sg", "age", "wt", "ekg")
for(i in var_na) data[[i]] <- imp[[i]]


# check sum(is.na)
sum(is.na(data))
check_sumisna <- function(data){ sum(is.na(data)) }
apply(data, FUN=check_sumisna, MARGIN=2)


# ------------------------------------------------------------------------------
# Transform data
# ------------------------------------------------------------------------------
data_trans <- as.data.frame(ptrans$transformed)
data_trans$cvd <- data$cvd

( var <- colnames(data_trans) )

psych::pairs.panels(data_trans[,var])
psych::pairs.panels(data[,var])


# ------------------------------------------------------------------------------
# Unlabel and subsetting:  data count 502 --> 226
# ------------------------------------------------------------------------------
# subsetting
idx <- which(data$status %in% c("dead - heart or vascular", "dead - carebrovascular", "dead - prostatic ca"))
data_s <- data[idx,]
data_sunlbl <- data[idx,]
data_trans_s <- data_trans[idx,]
data_trans_sunlbl <- data_trans[idx,]

# unlabell
for(i in 1:ncol(data_unlblo)) data_sunlbl[,i] <- unclass(data_sunlbl[,i])
for(i in 1:ncol(data_trans_unlblo)) data_trans_sunlbl[,i] <- unclass(data_trans_sunlbl[,i])

nrow(data)
nrow(data_s)
nrow(data_sunlbl)
nrow(data_trans_s)
nrow(data_trans_sunlbl)


# ------------------------------------------------------------------------------
# Dimension Reduction by Principal Components Analysis (prcomp)
# ------------------------------------------------------------------------------
# Variance explaned by PCs: Scree Plot  (showing x: PCs  y: individual variances explained  text:  cumulative)
addscree <- function(x, npcs=min(10, length(x$sdev)), plotv=FALSE, col=1, offset=0.8, adj=0, pr=FALSE, lwd=1, lty=1){
  vars <- x$sdev^2
  cumv <- cumsum(vars) / sum(vars)
  if(pr) print(cumv)
  text(1:npcs, vars[1:npcs] + offset * par('cxy')[2], as.character(round(cumv[1:npcs], 2)), srt=45, adj=adj, cex=0.65, xpd=NA, col=col)
  if(plotv) lines(1:npcs, vars[1:npcs], type="b", col=col, lwd=lwd, lty=lty)
}


# NOTE:  categorical variables are used as numeric value
pc.raw <- prcomp(~ sz + sg + log(ap) + sbp + dbp + age + wt + hg + ekg.norm + pfn + bm + hx + rxn + dtime, data = data_sunlbl, scale=T)
pc.trans <- prcomp(~ sz + sg + ap + sbp + dbp + age + wt + hg + ekg + pf + bm + hx + rx + dtime, data = data_trans_sunlbl, scale=T)

par(mfrow=c(1,1))
plot(pc.raw, type="lines", main="", ylim=c(0,3))
addscree(pc.raw, npcs=10)
addscree(pc.trans, npcs=10, plotv=TRUE, col="blue", offset=-0.8, adj=1, lwd=2, lty=2)


# --> raw data has almost flat curve in the middle, showing redundant dimensions


# ------------------------------------------------------------------------------
# Test for the significance of coefficients
# Univariale logistic regression
# ------------------------------------------------------------------------------
source("./utilities/UnivarLogiReg.R")

x_var <- c("rx","dtime","age","wt","pf","hx","sbp","dbp","ekg","hg","sz","sg","ap","bm")
y_var <- "cvd"

crit <- c(0.25, 0.05, 0.001)
uni_lr <- UnivarLogiReg(y_var = y_var, x_var = x_var, data = data_sunlbl, crit = crit)
uni_lr
# p.value of wt > 0.25
# It seems that ap needs transformation by log

uni_lr_trans <- UnivarLogiReg(y_var = y_var, x_var = x_var, data = data_trans_sunlbl, crit = crit)
uni_lr_trans
# p.value of rx, wt, pf, hg, bm > 0.25


# Check how bad is this univariate model, compared to a model (saturated model) that fits the data perfectly ?
i <- 1
x_var[i]
eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", x_var[i], ", data = data_sunlbl, family = binomial)")))
vcdExtra::LRstats(mod)


# ------------------------------------------------------------------------------
# Multivariable model with all covariates significant at the 0.25 level in the univariable analysis
# ------------------------------------------------------------------------------
source("./utilities/MultivarLogiReg.R")

y_var <- "cvd"
crit <- c(0.05, 0.001)


# extract only variable with p.val < 0.25 --> and select significant variables
# DO NOT FORGET applying log to age
uni_lr
x_var <- c("rx","dtime","log(age)","wt","pf","hx","sbp","dbp","ekg","hg","sz","sg","ap","bm")
x_var <- c("rx","dtime","log(age)","pf","hx","sbp","dbp","ekg","hg","sz","sg","ap","bm")
x_var <- c("rx","log(age)","hx","dbp","sz","sg","ap")
mult_lr <- MultivarLogiReg(y_var = y_var, x_var = x_var, data = data_sunlbl, crit = crit)
mult_lr["mult_lr"]
# retained variable:  rx, log(age), hx, dbp, sz, sg, ap  (from redundant analysis, R^2 for sg is high = 0.551, but RETAINED !!)


# extract only variable with p.val < 0.25 --> and select significant variables
uni_lr_trans
x_var <- c("rx","dtime","age","wt","pf","hx","sbp","dbp","ekg","hg","sz","sg","ap","bm")
x_var <- c("dtime","age","hx","sbp","dbp","ekg","sz","sg","ap")
x_var <- c("dtime","hx","sz")
mult_lr_trans <- MultivarLogiReg(y_var = y_var, x_var = x_var, data = data_trans_sunlbl, crit = crit)
mult_lr_trans["mult_lr"]
# retained variable: dtime, hx, sz


mult_lr["G.stat"]
mult_lr_trans["G.stat"]


# get model object
mod_raw_full <- glm(cvd ~ rx + dtime + log(age) + wt + pf + hx + sbp + dbp + ekg + hg + sz + sg + ap, data = data_sunlbl, family="binomial")
mod_raw_slc <- glm(cvd ~ rx + log(age) + hx + dbp + sz + sg + ap, data = data_sunlbl, family="binomial")
mod_trans_full <- glm(cvd ~ rx + dtime + age + wt + pf + hx + sbp + dbp + ekg + hg + sz + sg + ap, data = data_trans_sunlbl, family="binomial")
mod_trans_slc <- glm(cvd ~ dtime + hx + sz, data = data_trans_sunlbl, family="binomial")

modlrm_raw_full <- lrm(cvd ~ rx + dtime + log(age) + wt + pf + hx + sbp + dbp + ekg + hg + sz + sg + ap, data = data_s)
modlrm_raw_slc <- lrm(cvd ~ rx + log(age) + hx + dbp + sz + sg + ap, data = data_s)
modlrm_trans_full <- lrm(cvd ~ rx + dtime + age + wt + pf + hx + sbp + dbp + ekg + hg + sz + sg + ap, data = data_trans_s)
modlrm_trans_slc <- lrm(cvd ~ dtime + hx + sz, data = data_trans_s)


# ------------------------------------------------------------------------------
# variable selection by stepAIC()
#  --> AIC based model retained dtime, but almost same model
# ------------------------------------------------------------------------------
x_main_var <- c("rx","dtime","log(age)","wt","pf","hx","sbp","dbp","ekg","hg","sz","sg","ap","bm")
y_var <- "cvd"

mod_char <- paste0(x_main_var, collapse = " + ")

eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", mod_char, ", data = data_sunlbl, family = binomial)")))

step_aic <- stepAIC(mod, trace = TRUE, direction = "both")
step_bic <- stepAIC(mod, trace = TRUE, direction = "both", k = log(nrow(data_sunlbl)))

step_aic$anova
# retained variable: rx, dtime, log(age), hx, dbp, sz, sg, ap

step_bic$anova
# retained variable: rx, log(age), hx, sz, sg, ap


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


# Try transformed data
x_main_var <- c("rx","dtime","age","wt","pf","hx","sbp","dbp","ekg","hg","sz","sg","ap","bm")
y_var <- "cvd"

mod_char <- paste0(x_main_var, collapse = " + ")
eval(parse(text = paste0("mod <- glm(", y_var, " ~ ", mod_char, ", data = data_trans_sunlbl, family = binomial)")))
step_aic <- stepAIC(mod, trace = TRUE, direction = "both")
step_bic <- stepAIC(mod, trace = TRUE, direction = "both", k = log(nrow(data_trans_sunlbl)))
step_aic$anova
# retained variable: dtime, hx, sbp, sz, sg
step_bic$anova
# retained variable: dtime, hx, sg
anova(step_bic, step_aic, test="Chisq")


# get model object
mod_stepraw_aic <- glm(cvd ~ rx + dtime + log(age) + hx + dbp + sz + sg + ap, data = data_sunlbl, family="binomial")
mod_stepraw_bic <- glm(cvd ~ rx + log(age) + hx + sz + sg + ap, data = data_sunlbl, family="binomial")
mod_steptrans_aic <- glm(cvd ~ dtime + hx + sbp + sz + sg, data = data_trans_sunlbl, family="binomial")
mod_steptrans_bic <- glm(cvd ~ dtime + hx + sg, data = data_trans_sunlbl, family="binomial")

modlrm_stepraw_aic <- lrm(cvd ~ rx + dtime + log(age) + hx + dbp + sz + sg + ap, data = data_s)
modlrm_stepraw_bic <- lrm(cvd ~ rx + log(age) + hx + sz + sg + ap, data = data_s)
modlrm_steptrans_aic <- lrm(cvd ~ dtime + hx + sbp + sz + sg, data = data_trans_s)
modlrm_steptrans_bic <- lrm(cvd ~ dtime + hx + sg, data = data_trans_s)


# ------------------------------------------------------------------------------
# Variable selection by Fast Backwards Step-Down
# ------------------------------------------------------------------------------
x_main_var <- c("rx","dtime","log(age)","wt","pf","hx","sbp","dbp","ekg","hg","sz","sg","ap","bm")
y_var <- "cvd"

mod_char <- paste0(x_main_var, collapse = " + ")


# Fast Backwards Step-Down
eval(parse(text = paste0("modlrm <- lrm(", y_var, " ~ ", mod_char, ", data = data_sunlbl)")))

( fbw <- fastbw(modlrm) )

fbw$names.kept
# only log(age), hx, sz, sg is retained


# ------------------------------------------------------------------------------
# Model based on PC variable
# ------------------------------------------------------------------------------
source("./utilities/MultivarLogiReg.R")

# function to compute the first k PCs
ipc <- function(x, k=1, ...) prcomp(x, ..., scale=TRUE)$x[,1:k]


# NOTE that categorical variables are used in numeric variable
k = 8
pc8.raw <- ipc(~ sz + sg + log(ap) + sbp + dbp + age + wt + hg + ekg.norm + pfn + bm + hx + rxn + dtime, data = data_sunlbl, k = k)
pc8.raw <- data.frame(pc8.raw)
pc8.raw <- cbind(pc8.raw, cvd = data_sunlbl$cvd)

k = 8
pc8.raw2 <- ipc(~ sz + sg + log(ap) + sbp + dbp + age + wt + hg + ekg.norm + pfn + bm + hx + rxn + dtime, data = data_s, k = k)
pc8.raw2 <- data.frame(pc8.raw2)
pc8.raw2 <- cbind(pc8.raw, cvd = data_s$cvd)

y_var <- "cvd"
crit <- c(0.05, 0.001)

x_var <- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")
x_var <- c("PC1","PC2","PC4","PC8")
mult_lr_pc8raw <- MultivarLogiReg(y_var = y_var, x_var = x_var, data = pc8.raw, crit = crit)
mult_lr_pc8raw["mult_lr"]
# retained variable:  PC1, PC2, PC4, PC8


k = 8
pc8.trans <- ipc(~ sz + sg + ap + sbp + dbp + age + wt + hg + ekg + pf + bm + hx + rx + dtime, data = data_trans_sunlbl, k = k)
pc8.trans <- data.frame(pc8.trans)
pc8.trans <- cbind(pc8.trans, cvd = data_trans_sunlbl$cvd)

k = 8
pc8.trans2 <- ipc(~ sz + sg + ap + sbp + dbp + age + wt + hg + ekg + pf + bm + hx + rx + dtime, data = data_trans_s, k = k)
pc8.trans2 <- data.frame(pc8.trans2)
pc8.trans2 <- cbind(pc8.trans, cvd = data_trans_s$cvd)

y_var <- "cvd"
crit <- c(0.05, 0.001)

x_var <- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")
x_var <- c("PC1","PC2","PC3","PC5","PC8")
mult_lr_pc8trans <- MultivarLogiReg(y_var = y_var, x_var = x_var, data = pc8.trans, crit = crit)
mult_lr_pc8trans["mult_lr"]
# retained variable:  PC1, PC2, PC3, PC5, PC8


# get model object
mod_pc8raw_full <- glm(cvd ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data = pc8.raw, family="binomial")
mod_pc8raw_slc <- glm(cvd ~ PC1 + PC2 + PC4 + PC8, data = pc8.raw, family="binomial")
mod_pc8trans_full <- glm(cvd ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data = pc8.trans, family="binomial")
mod_pc8trans_slc <- glm(cvd ~ PC1 + PC2 + PC3 + PC5 + PC8, data = pc8.trans, family="binomial")

modlrm_pc8raw_full <- lrm(cvd ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data = pc8.raw2)
modlrm_pc8raw_slc <- lrm(cvd ~ PC1 + PC2 + PC4 + PC8, data = pc8.raw2)
modlrm_pc8trans_full <- lrm(cvd ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8, data = pc8.trans2)
modlrm_pc8trans_slc <- lrm(cvd ~ PC1 + PC2 + PC3 + PC5 + PC8, data = pc8.trans2)


# ------------------------------------------------------------------------------
# Also try restricted cubic splines transformation for continuous variables
# ------------------------------------------------------------------------------
modlrm_rcsraw_full <- lrm(cvd ~ rcs(sz,4) + rcs(sg,4) + rcs(log(ap),4) + rcs(sbp,4) + rcs(dbp,4) + rcs(age,4) + rcs(wt,4) + rcs(hg,4) + ekg + pf + bm + hx + rx + rcs(dtime,4), data = data_s)
modlrm_rcsraw_full


# ------------------------------------------------------------------------------
# Main effects + addition of interaction individually
# ------------------------------------------------------------------------------
source("./utilities/MainInterLogiReg.R")

x_main_var <- c("rx", "log(age)", "hx", "dbp", "sz", "sg", "ap")
x_add_inter_var <- c("rx", "log(age)", "hx", "dbp", "sz", "sg", "ap")
y_var <- "cvd"

crit <- c(0.10, 0.05, 0.001)

# it seems that interaction effect is small
main_inter_lr <- MainInterLogiReg(y_var = y_var, x_main_var = x_main_var, x_add_inter_var = x_add_inter_var, data = data_sunlbl, crit = crit)
main_inter_lr


# get model object
mod_raw_slc_inter <- glm(cvd ~ rx + log(age) + hx + dbp + sz + sg + ap + rx:dbp, data = data_sunlbl, family="binomial")
modlrm_raw_slc_inter <- lrm(cvd ~ rx + log(age) + hx + dbp + sz + sg + ap + rx:dbp, data = data_s)

summary(mod_raw_slc_inter)

anova(mod_raw_slc_inter, mod_raw_slc, test="Chisq")


# ------------------------------------------------------------------------------
# Model Comparison
#
# !!!! NOTE that reponse variable should be (1,0) or (TRUE, FALSE)
# ------------------------------------------------------------------------------
source("./utilities/ModelComparisonLogiReg.R")

mod_name <- c("mod_raw_full", "mod_raw_slc", "mod_trans_full", "mod_trans_slc", 
              "mod_stepraw_aic", "mod_stepraw_bic", "mod_steptrans_aic", "mod_steptrans_bic",
              "mod_pc8raw_full", "mod_pc8raw_slc", "mod_pc8trans_full", "mod_pc8trans_slc")

mod_list <- list(mod_raw_full, mod_raw_slc, mod_trans_full, mod_trans_slc, 
              mod_stepraw_aic, mod_stepraw_bic, mod_steptrans_aic, mod_steptrans_bic,
              mod_pc8raw_full, mod_pc8raw_slc, mod_pc8trans_full, mod_pc8trans_slc)

modlrm_name <- c("modlrm_raw_full", "modlrm_raw_slc", "modlrm_trans_full", "modlrm_trans_slc", 
                 "modlrm_stepraw_aic", "modlrm_stepraw_bic", "modlrm_steptrans_aic", "modlrm_steptrans_bic",
                 "modlrm_pc8raw_full", "modlrm_pc8raw_slc", "modlrm_pc8trans_full", "modlrm_pc8trans_slc", "modlrm_rcsraw_full")

modlrm_list <- list(modlrm_raw_full, modlrm_raw_slc, modlrm_trans_full, modlrm_trans_slc, 
                 modlrm_stepraw_aic, modlrm_stepraw_bic, modlrm_steptrans_aic, modlrm_steptrans_bic,
                 modlrm_pc8raw_full, modlrm_pc8raw_slc, modlrm_pc8trans_full, modlrm_pc8trans_slc, modlrm_rcsraw_full)


( modcomp <- ModCompLogiReg(mod_list = mod_list, mod_name = mod_name, lrm = FALSE) )

( modcomp_lrm <- ModCompLogiReg(mod_list = modlrm_list, mod_name = modlrm_name, lrm = TRUE) )



# ------------------------------------------------------------------------------
# Basics of Fitted Model
# Describing the Fitted Model
# ------------------------------------------------------------------------------
mod <- mod_raw_slc
modlrm <- modlrm_raw_slc

dd <- datadist(data_s);  options(datadist = "dd")

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


# Nomogram:  logodds
# Nomogram:  logodds and probability
par(mfrow=c(1,1))
plot(nomogram(modlrm))
plot(nomogram(modlrm, fun=plogis, funlabel="Probability", fun.at=c(.01, .05, .1, .25, .5, .75, .9, .95, .99)))



# ------------------------------------------------------------------------------
# Effect plots
#  -- and partial residual plots --> checking indication of nonlinearlity
# ------------------------------------------------------------------------------
mod <- mod_raw_slc
modlrm <- modlrm_raw_slc

dd <- datadist(data_s);  options(datadist = "dd")


# Main effect (default is response probabilities)
eff <- effects::allEffects(mod, partial.residuals = TRUE)
names(eff)

eff[["sg"]]
eff[["sg"]]$model.matrix %>% head()


# plot main effets of each variable (default is reponse probabilities)
plot(eff)
plot(eff, type = "response")


# plot main effects and partial residuals for numeric variables with smoothed loess curve
plot(eff, rows = 2, cols = 2, type = "response", residuals.pch = 15)


