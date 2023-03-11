# ------------------------------------------------------------------------------
# Data: ari
#
# pneumonia（肺炎）,  meningitis（髄膜炎）,  sepsis（敗血症）の severity
# CSF (cerebrospinal fluid　脳脊髄液)
# LP (lumbar puncture)
# BC (blood culture 血液培養)
# SaOs (arterial oxygen saturation 動脈血酸素飽和度)
# CXR (chest X-ray)
# 生後90日以内の　4552 の乳幼児　が対象
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "vcd", "popbio", "effects", "MASS", "car")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# Load Data and Basics
# ------------------------------------------------------------------------------
getHdata(ari)

data <- ari
data_sc <- Sc

dim(data)
dim(data_sc)

Hmisc::describe(data)
Hmisc::describe(data_sc)
Hmisc::describe(Y)

dd <- datadist(data);  options(datadist="dd")

# check missing values  --> already missing value is imputed
mis_n <- function(x) sum(is.na(x))
apply(data, MARGIN=2, FUN=mis_n)
apply(data_sc, MARGIN=2, FUN=mis_n)


# ------------------------------------------------------------------------------
# Classification of variables
# ------------------------------------------------------------------------------
unique_n <- function(x) length(unique(x))
( unique_n_byvar <- apply(data, MARGIN=2, FUN=unique_n) )


# binary variable
( var_bin <- names(unique_n_byvar)[unname(unique_n_byvar) == 2] )


# category variable
( var_cat <- names(unique_n_byvar)[unname(unique_n_byvar) >= 3 & unname(unique_n_byvar) <= 11] )


# continuous variable
( var_cont <- setdiff(names(data), c("stno", var_bin, var_cat)) )


length(var_bin);  length(var_cat);  length(var_cont);


# variable of clinical signs: 49
# 49 clinical signs were collected for each infant. Most questionnaire items were scored as a single variable using equaly spaced codes, with 0 to 3 representing.
var_clinicalsigns <- c("abb", "convul", "abk", "hdi", "deh", "stu", "dcp", "hcl", "qcr", "csd", 
                      "slpm", "wake", "aro", "mvm", "hcm", "slpl", "con", "csa", "hcm", "hcs", 
                      "qcr", "smi2", "nfl", "lcw", "gru", "ccy", "hap", "apn", "whz", "coh", 
                      "crs", "hfb", "hdb", "hlt", "hfa", "absu", "afe", "chi", "fde", "ldy", 
                      "twb", "abd", "jau", "omph", "illd", "hfe", "conj", "oto", "puskin")


# ------------------------------------------------------------------------------
# The relationship of numeric variables X against Y
# Margianl plots
# ------------------------------------------------------------------------------
var <- var_cont

tmp <- cbind(data[,var],Y)

psych::pairs.panels(tmp, bg=c("blue", "yellow", "red")[tmp$Y])


# Margianl plots to check relationship between y and x
op <- par(mfrow=c(2,2), mar = c(4,4,1,2.5) + .1 , cex.lab = 1.4)
col <- c("lightblue", "blue", "salmon")
par(mfrow=c(2,2))
for(i in 1:length(var)){ eval(parse(text = paste0("plot(Y ~ ", var[i], ", data = data, col = col)"))) }



# ------------------------------------------------------------------------------
# Association of each categorical and factor variable X against Y
# doubledecker
# ------------------------------------------------------------------------------
catfac_var <- var_clinicalsigns

# doubledecker
for(i in 1:length(catfac_var)){
  eval(parse(text = paste0("vcd::doubledecker(Y ~ ", catfac_var[i], ", data = data)")))
}


# ------------------------------------------------------------------------------
# create unlabelled data
# ------------------------------------------------------------------------------
data_unlbl <- cbind(data,Y)

for(i in 1:ncol(data_unlbl)) data_unlbl[,i] <- unclass(data_unlbl[,i])


# ------------------------------------------------------------------------------
# Redundancy Analysis CANNOT be applied, since it is only binary or 3 values data
# ------------------------------------------------------------------------------
char0 <- paste0(var_clinicalsigns, collapse=" + ")

r2_cutoff <- 0.3
eval(parse(text = paste0("redun(~ ", char0, ", r2 = r2_cutoff, type='adjusted', data = data)")))


# ------------------------------------------------------------------------------
# Variable Clustering
#
# General and robust similarity measure is Hoeffding's D
# D detects nonmonotonic associations
# ------------------------------------------------------------------------------

char0 <- paste0(var_clinicalsigns, collapse=" + ")

# using Spearman's rho^2
eval(parse(text = paste0("vclust_s <- varclus(~ ", char0, ", data = data)")))
# using Hoeffding's D
eval(parse(text = paste0("vclust_h <- varclus(~ ", char0, ", data = data, sim='hoeffding')")))

plot(vclust_s)
plot(vclust_h)

vclust_s
vclust_h


# ------------------------------------------------------------------------------
# create numerical data for PC
# ------------------------------------------------------------------------------
data_num <- data

# transform to numerical data
for(var in var_clinicalsigns) data_num[,var] <- as.numeric(data[,var])

str(data_num)


# ------------------------------------------------------------------------------
# Dimension Reduction by Principal Components Analysis (prcomp)
# ------------------------------------------------------------------------------
# Variance explaned by PCs: Scree Plot  (showing x: PCs  y: individual variances explained  text:  cumulative)
addscree <- function(x, npcs=min(20, length(x$sdev)), plotv=FALSE, col=1, offset=0.8, adj=0, pr=FALSE, lwd=1, lty=1){
  vars <- x$sdev^2
  cumv <- cumsum(vars) / sum(vars)
  if(pr) print(cumv)
  text(1:npcs, vars[1:npcs] + offset * par('cxy')[2], as.character(round(cumv[1:npcs], 2)), srt=45, adj=adj, cex=0.65, xpd=NA, col=col)
  if(plotv) lines(1:npcs, vars[1:npcs], type="b", col=col, lwd=lwd, lty=lty)
}


# NOTE:  categorical variables are used as numeric value
char0 <- paste0(var_clinicalsigns, collapse=" + ")
eval(parse(text = paste0("pc <- prcomp(~", char0, ", data = data_num, scale=T)")))
summary(pc)
biplot(pc)

par(mfrow=c(1,1))
plot(pc, type="lines", main="", ylim=c(0,10), 20)
addscree(pc, npcs=20, pr=TRUE)


# ------------------------------------------------------------------------------
# Graphic Assessment of Proportional Odds Assumption
# ------------------------------------------------------------------------------
var <- colnames(data_sc)
( char0 <- paste0(var, collapse = " + ") )


# CRモデルでの期待値が "C" でマーク
par(mfrow=c(3,4))
eval(parse(text = paste0("plot.xmean.ordinaly(Y ~ ", char0, ",data=data_sc, cr=TRUE, subn=FALSE, lwd = 1, pch = 16, cex.points=.65)")))


data_sc2 <- data_sc
data_sc2 <- data_sc %>% mutate(
  ausc = 1 * (ausc == 3),
  bul.conv = 1 * (bul.conv == "TRUE"),
  abdominal = 1 * (abdominal == "TRUE"))


# check of the variable with expected strongly nonlinear effects
hist(data_sc2)
summary(data_sc2$temp)
summary(data_sc2$hrat)

par(mfrow=c(1,3))
plot.xmean.ordinaly(Y ~ temp, data = data_sc2, cr=TRUE, subn = FALSE, cex.points=0.5)
plot.xmean.ordinaly(Y ~ abs(temp-mean(temp)), data = data_sc2, cr=TRUE, subn = FALSE, cex.points=0.5)
plot.xmean.ordinaly(Y ~ abs(temp-37), data = data_sc2, cr=TRUE, subn = FALSE, cex.points=0.5)


summary(data_sc2$hrat)
par(mfrow=c(1,1));  hist(data_sc2, breaks=seq(0,300,10))
plot(Y ~ hrat, data = data_sc2, col=c("lightblue", "salmon", "lightgreen"))
par(mfrow=c(1,3))
plot.xmean.ordinaly(Y ~ hrat, data = data_sc2, cr=TRUE, subn = FALSE, cex.points=0.5)
plot.xmean.ordinaly(Y ~ abs(hrat-mean(hrat)), data = data_sc2, cr=TRUE, subn = FALSE, cex.points=0.5)
plot.xmean.ordinaly(Y ~ abs(hrat-125), data = data_sc2, cr=TRUE, subn = FALSE, cex.points=0.5)


plot.xmean.ordinaly(Y ~ age + abs(temp-37) + abs(rr-60) + abs(hrat-125) + waz + bul.conv + drowsy + agitated + reffort + ausc + feeding + abdominal,
                    data = data_sc2, cr=TRUE, subn = FALSE, cex.points=0.5)


# ------------------------------------------------------------------------------
# discretize variables and missing value imputation
#
# Missing value of original data (Sc) is already imputed
# ------------------------------------------------------------------------------
# age は temp, rr, hrat の解釈を変えてしまう可能性があること、連続変数間で交互作用を含めると解釈が難しくなる可能性があるため、
# age を離散変数化する
data_sc2$ageg <- cut2(data_sc2$age, c(7,60))

#
( vsign.trans <- transcan(~ temp + hrat + rr, data=Sc, imputed=TRUE, pl=FALSE) )

#
Sc <- transform(Sc, temp = impute(vsign.trans, temp), hrat=impute(vsign.trans, hrat), rr=impute(vsign.trans, rr))


# ------------------------------------------------------------------------------
# Numerical Assessment of Proportional Odds Assumption
# ------------------------------------------------------------------------------
data_sc2 <- cbind(data_sc2, Y)

sf <- function(y) {
  c('Y>=0' = qlogis(mean(Y >= 0)),
    'Y>=1' = qlogis(mean(Y >= 1)),
    'Y>=2' = qlogis(mean(Y >= 2)))
}

# the (linear) predicted values (logit) if we regressed our dependent variable on our predictor variables one at a time, without the parallel slopes assumption. 
(s <- with(data_sc2, summary(as.numeric(Y) ~ ageg + temp + rr + hrat + waz + bul.conv + drowsy + agitated + reffort + ausc + feeding + abdominal, fun=sf)))


# Doing same ...
glm(I(as.numeric(Y) >= 2) ~ ageg, family="binomial", data = data_sc2)
glm(I(as.numeric(Y) >= 2) ~ temp, family="binomial", data = data_sc2)


s[,4] <- s[,4] - s[,3]
s[,3] <- s[,3] - s[,3]
s

par(mfrow=c(1,1))
plot(s, which = 1:3, pch = 1:3, xlab = "logit", main="", xlim = range(s[,3:4]))


# ------------------------------------------------------------------------------
# Tentative Full Proportioanl Odds Model
# ------------------------------------------------------------------------------
modpo_polr <- polr(as.factor(Y) ~ ageg*(rcs(temp,5) + rcs(rr,5) + rcs(hrat,4)) +
                   rcs(waz,4) + bul.conv + drowsy + agitated +
                   reffort + ausc + feeding + abdominal, data=data_sc2)

modpo_polr
# test for coefficients and intercept
car::Anova(modpo_polr)


# WHY ERROR ...?
eff <- allEffects(modpo_polr, partial.residuals = TRUE)



modpo_lrm <- lrm(Y ~ ageg*(rcs(temp,5) + rcs(rr,5) + rcs(hrat,4)) +
          rcs(waz,4) + bul.conv + drowsy + agitated +
          reffort + ausc + feeding + abdominal,
          data=data_sc2, x=TRUE, y=TRUE)

print(modpo_lrm, coefs=5)

# check relative contribution of interaction ageg with modifier tmo, rr, and hrat
anova(modpo_lrm)


# ------------------------------------------------------------------------------
# Assessment of model fil: score residual plot
#
# Binary Logistic Score Residuals for isolating the PO assumption in an ordinal model
# ------------------------------------------------------------------------------
par(mfrow=c(4,4))
resid(modpo_lrm, "score.binary", pl=TRUE)
resid(modpo_lrm, "score.binary", pl=TRUE, which=c(17,18,20,21))

# --> strong evidence of non-PO for ausc and moderate evidence for drowsy and bul.conv


# ------------------------------------------------------------------------------
# Assessment of model fit:  partial residual plot  --> CAN BE USED UNIVARIABLE CHECK
# 
# partial residuals allow examination of predictor transformations (linealiry) while simultaneously allowin examination of PO (parallelism)
# ------------------------------------------------------------------------------
mod0 <- lrm(Y ~ age + temp + rr + hrat + waz +
          bul.conv + drowsy + agitated + reffort + ausc +
          feeding + abdominal, data=data_sc, x=TRUE, y=TRUE)

par(mfrow=c(4,3))
resid(mod0, 'partial', pl=TRUE, label.curves=FALSE)


# ------------------------------------------------------------------------------
# Graphical Assessment of Fit of CR Model
#
# smoothed partial residuals corresponding to two cutoffs of Y, from a model in which all predictors were assumed to operate linearly and additively.
# ------------------------------------------------------------------------------
modlrm_cr0 <- lrm(Y==0 ~ age + temp + rr + hrat + waz + bul.conv + drowsy + agitated + reffort + ausc + feeding,
                  data = data_sc2, x = TRUE, y = TRUE)

modlrm_cr1 <- update(modlrm_cr0, Y==1 ~ ., subset = Y>=1)


# assemble partial residuals for a sequence of fits and constructs one graph per predictor
# There is not much more parallelism here than PO model
# For the two most important predictors, ausc and rr, there are strongly differing effects for the different evens being predicted.
plot.lrm.partial(modlrm_cr0, modlrm_cr1, center = TRUE)
resid(mod0, 'partial', pl=TRUE, label.curves=FALSE)


# ------------------------------------------------------------------------------
# Extended Continuation Ratio Model
# ------------------------------------------------------------------------------
# cr.setup function to set up the data for fitting a CR model using the binary logistic trick
u <- cr.setup(Y)

data_sc_expanded <- data_sc2[u$subs, ]
y <- u$y
cohort <- u$cohort

Y[1:10];  y[1:10];  cohort[1:10]

perf <- function(fit){
  pr <- predict(fit, type="fitted")[cohort == "all"]
  s <- round(somers2(pr, y[cohort == "all"]), 3)
  pr <- 1 - pr
  f <- round(c(mean(pr < 0.05), mean(pr > 0.25), mean(pr > 0.5)), 2)
  f <- paste(f[1], ",", f[2], ", and ", f[3], ".", sep=" ")
  list(somers=s, fractions=f)
}


modlrm_full <- lrm(y ~ cohort*(ageg*(rcs(temp,5) + rcs(rr,5)) + rcs(waz,4) + bul.conv + drowsy + agitated + reffort + ausc + feeding + abdominal + hydration + hxprob +
                                 pustular + crying + fever.ill + stop.breath + labor), data = data_sc_expanded, x=TRUE, y=TRUE)

print(modlrm_full, coefs=FALSE)
anova(modlrm_full, cohort)


# Wald statistics for the continuation ratio model. Interaction with cohort assess non-proportional hazards
# The test indicate that ausc is the biggest violater, followed by waz and rr ... ??
print(modlrm_full)

perf(modlrm_full)


# ------------------------------------------------------------------------------
# Penalized Estimation
#
# In order to secure precision or power, we seek some restrictions on the model's parameters.  (differing lambda for shrinking different types of terms in the model)
# ------------------------------------------------------------------------------

# Grid search to determine the optimum penalty for simple main effect(non-interaction) terms and the penalty for interaction terms,
# most of which are terms interacting with cohort
d <- options(digits = 4)
pentrace(modlrm_full, list(simple=c(0, 0.25, 0.05, 0.075, 0.1), interaction=c(0, 10, 50, 100, 125, 150)))

# We see that shrinkage from 87 df down to 49.75 effective df results in an improvement in chi^2 scaled AIC of 23

# update model by best penalty
simple <- 0.05;  inter <- 125;
modlrm_full_pen <- update(modlrm_full, penalty=list(simple=simple, interaction=inter))

print(modlrm_full_pen, coefs=FALSE)

# non-interation terms have barely been penalized and coeefs of interaction terms have been shurunken from 59 df to effectively 22.6 df.
effective.df(modlrm_full_pen)

# the fraction of infants with predicted probabilities that Y>0 being <0.05, >0.25 and >0.5 are, respectively, 0.1, 0.28, and 0.14.
perf(modlrm_full_pen)

# Wald statistics used here are computed on a variance-covariance matrix which is adjusted for penalization
# excluding interactions and cohort effects from plot
par(mfrow=c(1,1))
plot(anova(modlrm_full_pen), cex.labels=0.75, rm.ia=TRUE, rm.other="cohort(Factor+Higher Order Factors)")


# ------------------------------------------------------------------------------
# Disply of the shapes of effects of the predictors
# ------------------------------------------------------------------------------

dd <- datadist(data_sc_expanded);  dd <- datadist(dd, cohort)
options(datadist = "dd")


# plot predictors that interact with another predictor
# Vary ageg over all age groups, then vary temp over its data,
# ref.zero centers effects using median x
# tile "all" referes to the prediction of Y=0|Y>=0, that is Y=0
# tile "Y>=1" referes to the prediction of Y=1|Y>=1
p1 <- Predict(modlrm_full_pen, temp, ageg, cohort, ref.zero=TRUE, conf.int=FALSE)
p2 <- Predict(modlrm_full_pen, rr, ageg, cohort, ref.zero=TRUE, conf.int=FALSE)
p <- rbind(temp=p1, rr=p2)
ggplot(p, ~ cohort, groups="ageg", varypred=TRUE, ylim=c(-3,1), layout=c(2,1), legend.position=c(0.85, 0.8), adj.subtitle=FALSE)


# For each predictor that only interacts with cohort,
# plot the differing effects of the predictor for predicting Pr(Y=0) and pr(Y=1 given Y excedds 0) on the same graph
v <- c("waz", "bul.conv", "drowsy", "agitated", "reffort", "ausc", "feeding", "abdominal", "hydration", "hxprob", "pustular", "crying")
dd$limits["Adjust to", "cohort"] <- "Y>=1"
yeq1 <- Predict(modlrm_full_pen, name=v, ref.zero=TRUE)
ggplot(yeq1, ylim=c(-3,1), sepdiscrete="vertical")

dd$limits["Adjust to", "cohort"] <- "all"
all <- Predict(modlrm_full_pen, name=v, ref.zero=TRUE)
ggplot(all, ylim=c(-3,1), sepdiscrete="vertical")


# ------------------------------------------------------------------------------
# Validating the model
#
# check the optimism level
# ------------------------------------------------------------------------------
set.seed(2018)

v <- validate(modlrm_full_pen, B=200, cluster=u$subs, subset=cohort=="all")
v

# --> bias-corrected estimate of predictive discrimination Dxy = 0.6604
# The intercept and slope needed to re-calibrate X * beta to a 45 degree line are very near (0, 1) !!!!


# ------------------------------------------------------------------------------
# Overfitting-corrected calibration curve
# ------------------------------------------------------------------------------

# estimtate an overfitting-corrected calibration curve nonparametrically
cal <- calibrate(modlrm_full_pen, B=200, cluster=u$subs, subset=cohort=="all")
cal
err <- plot(cal)




