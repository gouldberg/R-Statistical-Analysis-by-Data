# ------------------------------------------------------------------------------
# Estimate Time to Death (months to follow-up)
#
# try:
#  - variable clustering
#  - dimension reduction by PCA and sparse PCA
#  - Cox Proportional Hazard Regression Model
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

# describe
describe(data)

# object variable  -->  NOTE that 354 deaths among 502 patients
describe(data$status)
table(data$status)


# How many parameters can be estimated ?  (Rules of thumb:  RMS p.72)  --> only 24 parameters examined against Y in unpenalized modeling
m <- 15  # limiting sample size
nrow(data %>% filter(status != "alive")) / m


# again check describe --> variable with low frequency but requiring high DFs is "rx", "pf", "ekg"
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
# Redundancy Analysis
#
# None of the predictors is redundant. stage can be predicted with R^2 = 0.658 from other 13 variables,
# but only with R^2 = 0.493 after deletion of 3 variables lated declared to be redundant.
# ------------------------------------------------------------------------------
# Allow only 1 d.f. for "rx", "pf", "ekg" (variables with low frequency and high DFs)
data_s <- transform(prostate,
                    ekg.norm = 1 * (ekg %in% c("normal", "benign")),
                    rxn = as.numeric(rx),
                    pfn = as.numeric(pf))

str(data_s)

r2_cutoff <- 0.3
redun(~ stage + I(rxn) + age + wt + I(pfn) + hx + sbp + dbp + ekg.norm + hg + sz + sg + ap + bm, r2 = r2_cutoff, type="adjusted", data = data_s)


# ------------------------------------------------------------------------------
# Variable Clustering
#
# General and robust similarity measure is Hoeffding's D
# D detects nonmonotonic associations
# ------------------------------------------------------------------------------
# data_s has list type, so do not use dplyr::select()
x <- with(data_s, cbind(stage, rx, age, wt, pf, hx, sbp, dbp, ekg.norm, hg, sz, sg, ap, bm))
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


# Variable Clustering:  we combine sbp and dbp, and tentatively combine ap, sg
vc <- varclus(~ stage + rxn + age + wt + pfn + hx + sbp + dbp + ekg.norm + hg + sz + sg + ap + bm, sim="hoeffding", data=data_s)
plot(vc)


# ------------------------------------------------------------------------------
# Simultaneous transformation and single imputation of all candidate predictors
#
# R transcan() uses a maximum generalized variance method (MGV) that incorporates canonical variates to optimally transform both sides of a multiple regression model.
# Each predictor is treated in turn as a variable being predicted, and all variables are expanded into restricted cubic splines (for continuous variables) or
# dummy variables (for categorical ones)
# ------------------------------------------------------------------------------
levels(data$ekg)[levels(data$ekg) %in% c('old MI', 'recent MI')] <- 'MI'
data$pf.coded <- as.integer(data$pf)
levels(data$pf) <- levels(data$pf)[c(1,2,3,3)]

# Simultaneous transformation and single imputation of all candidate predictors
ptrans <- transcan(~ sz + sg + ap + sbp + dbp + age + wt + hg + ekg + pf + bm + hx,
                   imputed = TRUE, transformed = TRUE, trantab = TRUE, pl = FALSE, show.na = TRUE, data = data, frac = 0.1, pr = FALSE)

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
ggplot(ptrans, scale=TRUE) + theme(axis.text.x = element_text(size=6))

# CHECK LINEALITY !!!
# --> imputed and transformed "ap" requires still additional log transformation

# Impute all missing values in all variables given to transcan
imputed <- impute(ptrans, data=prostate, list.out=TRUE)
imputed <- as.data.frame(imputed)

## FOR REFERENCE:  transformation using Nonparametric Smoothers (ACE nonparametric additive regression)
# NOTE:  transace does not impute, but merely does casewise deletion
x <- with(imputed, cbind(sz, sg, ap, sbp, dbp, age, wt, hg, ekg, pf, bm, hx))
monotonic <- c("sz", "sg", "ap", "sbp", "dbp", "age", "pf")
x_transace <- transace(x, monotonic, categorical="ekg", binary=c("bm", "hx"))


# ------------------------------------------------------------------------------
# Dimension Reduction by Principal Components Analysis (prcomp)
# ------------------------------------------------------------------------------
# Before principal components analysis, create design matrix for Ekg (categories)
Ekg <- model.matrix(~ ekg, data = imputed)[,-1]

imputed <- cbind(imputed, Ekg)
imputed <- imputed %>% mutate(pfn = data_s$pfn)
str(imputed)

# principal component analysis for raw data and transformed data
prin.raw <- prcomp(~ sz + sg + ap + sbp + dbp + age + wt + hg + Ekg + pfn + bm + hx, scale=T, data=imputed)
# prin.raw2 <- princomp(~ sz + sg + ap + sbp + dbp + age + wt + hg + Ekg + pfn + bm + hx, cor=T, data=imputed)
prin.trans <- prcomp(ptrans$transformed, scale=T)

# Variance explaned by PCs: Scree Plot  (showing x: PCs  y: individual variances explained  text:  cumulative)
addscree <- function(x, npcs=min(10, length(x$sdev)), plotv=FALSE, col=1, offset=0.8, adj=0, pr=FALSE, lwd=1, lty=1){
  vars <- x$sdev^2
  cumv <- cumsum(vars) / sum(vars)
  if(pr) print(cumv)
  text(1:npcs, vars[1:npcs] + offset * par('cxy')[2], as.character(round(cumv[1:npcs], 2)), srt=45, adj=adj, cex=0.65, xpd=NA, col=col)
  if(plotv) lines(1:npcs, vars[1:npcs], type="b", col=col, lwd=lwd, lty=lty)
}

par(mfrow=c(1,1))
plot(prin.raw, type="lines", main="", ylim=c(0,3))
addscree(prin.raw)
addscree(prin.trans, npcs=10, plotv=TRUE, col="blue", offset=-0.8, adj=1, lwd=2, lty=2)


# --> raw data has almost flat curve in the middle, showing redundant dimensions


# ------------------------------------------------------------------------------
# Cox regression for dtime by PCs
# 
# Compare:
# 1.  Cox regression for raw data PCs, raw data, raw + rcs full
# 2.  Cox regression for imputed/transformed and CLUSTERED VARIABLE's first PCs
# ------------------------------------------------------------------------------
library("survival")

# Objective for Cox regression for dtime
S <- with(data, Surv(dtime, status != "alive"))

# 1. Cox regression by PC1 - 16 and get AIC for each model
aic <- numeric(16)
for(i in 1:16){
  ps <- prin.raw$x[,1:i]
  aic[i] <- AIC(cph(S ~ ps))
}

# 1-2. Cox regression by raw data, and raw + rcs full model
f <- cph(S ~ sz + sg + log(ap) + sbp + dbp + age + wt + hg + ekg + pf + bm + hx, data = imputed)
f_rcs <- cph(S ~ rcs(sz, 5) + rcs(sg, 5) + rcs(log(ap), 5) + rcs(sbp, 5) + rcs(dbp, 5) + rcs(age, 3) + rcs(wt, 5) + rcs(hg, 5) + ekg + pf + bm + hx, tol=1e-14, data = imputed)

# plot AIC by model and raw model (raw and raw + rcs full spline model)
# NOTE:  ap is requires additional log transformation
# --> only five PCs are better than raw model

plot(1:16, aic, xlab="Number of Components Used", ylab="AIC", type="l", ylim=c(3950, 4000))
abline(h = AIC(f), col="red")
abline(h = AIC(f_rcs), col="blue", lty=2)

# best of 1 is:
ps <- prin.raw$x[,1:5]
f_1best <- cph(S ~ ps)


# FUNCTION:  dimension reduction for imputed and transformed and get first PC scores by clustered variables
pco <- function(v){
  f <- prcomp(ptrans$transformed[,v], scale=T)
  vars <- f$sdev^2
  cat("Fraction of variance explained by PC1:", round(vars[1]/sum(vars),2), "\n")
  f$x[,1]
}

tumor <- pco(c("sz", "sg", "ap", "bm"))
bp <- pco(c("sbp", "dbp"))
cardiac <- pco(c("hx", "ekg"))

other <- ptrans$transformed[,c("hg","age","pf","wt")]

# 2. Cox regression for imputed/transformed and CLUSTERED VARIABLE's first PCs
f_cluspc <- cph(S ~ tumor + bp + cardiac + other)
AIC(f_cluspc)


# Compare models  -->  Imputed/transformed and CLUSTERED VARIABLE's fist PCs are BEST MODEL in AIC
# Also, "tumor" and "cardiac" (both are clusterd variable) is significant and dominate prediction, with positive coefficients
AIC(f_1best);  AIC(f);  AIC(f_rcs);  AIC(f_cluspc);

print(f_1best)
print(f)
print(f_rcs)
print(f_cluspc)


# ------------------------------------------------------------------------------
# Sparse principal Components by pcaPP package  -->  we can see variable clustering TOO !!!
#
# we use transformed predictors to allow for nonlinear transformations and to score the ekg variable to a scalar
# ------------------------------------------------------------------------------
library(pcaPP)

# sparse principal components
k <- 10;  maxiter <- 10;
s <- sPCAgrid(ptrans$transformed, k = k, method = "sd", center = mean, acaled = sd, scores = TRUE, maxiter = maxiter)

plot(s, type="lines", main="", ylim=c(0,3))
addscree(s)

# We can see variable clustering too here.
s$loadings


aic <- numeric(k)
for(i in 1:k){
  ps <- s$scores[,1:i]
  aic[i] <- AIC(cph(S ~ ps))
}

# BUT requires more PCs than non-sparse PC model
plot(1:k, aic, xlab="Number of Components used", ylab="AIC", type="l", ylim=c(3950,4000))






