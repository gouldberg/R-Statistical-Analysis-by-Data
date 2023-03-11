# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//election")

packages <- c("dplyr", "Hmisc")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  election
# ------------------------------------------------------------------------------

library(poLCA)

data(election)

str(election)



# ----------
dat <- election




# ------------------------------------------------------------------------------
# Latent Class Model by poLCA()
#   - As default, missing values are removed (listwise deleted) before estimating the model
# ------------------------------------------------------------------------------

# poLCA() can estimate complex models with covariates, but here we only wish to examine the effect of cluster membership alone.
# Thus, we model the dependent variables (all the observed columns) with respect to the model intercepts
# ~1: to specify a formula with intercepts only

f <- cbind(MORALG, CARESG, KNOWG, LEADG, DISHONG, INTELG, MORALB, CARESB, KNOWB, LEADB, DISHONB, INTELB) ~ 1


nes3 <- poLCA(f, dat, nclass = 3, na.rm = FALSE)
nes5 <- poLCA(f, dat, nclass = 5, na.rm = FALSE, graphs = TRUE)
nes6 <- poLCA(f, dat, nclass = 6, na.rm = FALSE)
nes7 <- poLCA(f, dat, nclass = 7, na.rm = FALSE)
nes8 <- poLCA(f, dat, nclass = 8, na.rm = FALSE)
nes9 <- poLCA(f, dat, nclass = 9, na.rm = FALSE)
nes11 <- poLCA(f, dat, nclass = 11, na.rm = FALSE)


nes3$bic;  nes5$bic;  nes6$bic;  nes7$bic;  nes8$bic;  nes9$bic;  nes11$bic


# --> 
# number of groups = 8 is best in terms of BIC



# ------------------------------------------------------------------------------
# Latent Class Model by poLCA() with a single covariate (PARTY)
#   - other covariates (GENDER, EDUC, AGE was not highly significant)
# ------------------------------------------------------------------------------

table(dat$PARTY)



f2a <- cbind(MORALG, CARESG, KNOWG, LEADG, DISHONG, INTELG, MORALB, CARESB, KNOWB, LEADB, DISHONB, INTELB) ~ PARTY


# nrep: number of times to estimate the model, using different values of probs.start (the default is one)
# setting nrep > 1 automates the search for the global -- rather tahn just a local -- maximum of the log-likelihood function
nes2_2 <- poLCA(f2a, dat, nclass = 2, nrep = 5, na.rm = TRUE)
nes2_3 <- poLCA(f2a, dat, nclass = 3, nrep = 5, na.rm = TRUE)
nes2_5 <- poLCA(f2a, dat, nclass = 5, nrep = 5, na.rm = TRUE)
nes2_6 <- poLCA(f2a, dat, nclass = 6, nrep = 5, na.rm = TRUE)
nes2_7 <- poLCA(f2a, dat, nclass = 7, nrep = 5, na.rm = TRUE)
nes2_8 <- poLCA(f2a, dat, nclass = 8, nrep = 5, na.rm = TRUE)
# nes2_9 <- poLCA(f2a, dat, nclass = 9, nrep = 5, na.rm = TRUE)
# nes2_11 <- poLCA(f2a, dat, nclass = 11, nrep = 5, na.rm = TRUE)


nes2_2$bic;  nes2_3$bic;  nes2_5$bic;  nes2_6$bic;  nes2_7$bic;  nes2_8$bic;
# nes2_9$bic;  nes2_11$bic;


# -->
# number of groups = 5 is best in terms of BIC
# Also notice that PARTY is significant 



# ----------
# we apply the number of groups = 3
nes2_3$coeff


pidmat <- cbind(1, c(1:7))
exb <- exp(pidmat %*% nes2_3$coeff)
matplot(c(1:7), (cbind(1, exb)/(1 + rowSums(exb))), ylim = c(0, 1), type = "l", 
        main = "Party ID as a predictor of candidate affinity class",
        xlab = "Party ID: strong Democratic (1) to strong Republican (7)",
        ylab = "Probability of latent class membership", lwd = 2, col = 1)

text(5.9, 0.35, "Other")
text(5.4, 0.7, "Bush affinity")
text(1.8, 0.6, "Gore affinity")



# ----------
# we apply the number of groups = 5
nes2_5$coeff


pidmat <- cbind(1, c(1:7))
exb <- exp(pidmat %*% nes2_5$coeff)
matplot(c(1:7), (cbind(1, exb)/(1 + rowSums(exb))), ylim = c(0, 1), type = "l", 
        main = "Party ID as a predictor of candidate affinity class",
        xlab = "Party ID: strong Democratic (1) to strong Republican (7)",
        ylab = "Probability of latent class membership", lwd = 2, col = 1)



# ---------------------------------------------------------------------------
# Model comparison
# ---------------------------------------------------------------------------

nes8$bic
nes2_5$bic


# -->
# The model with PARTY covariate and the number of groups = 5 is best



# ---------------------------------------------------------------------------
# check the clusters variation
# ---------------------------------------------------------------------------
# Note that the class was not estimated for some individuals (although na.rm = TRUE was applied..., only complete cases are applied)

nrow(dat)

length(nes2_5$predclass)

nrow(dat[complete.cases(dat[,c(1:12,17)]),])

dat_comp <- dat[complete.cases(dat[,c(1:12,17)]),]



# ----------
# segment group statistics
seg.summ <- function(data, groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


table(nes2_5$predclass)

seg.summ(dat_comp, nes2_5$predclass)


# ----------
dat_comp$cluster <- nes2_5$predclass


library(lattice)


var_n <- c("MORALG", "CARESG", "KNOWG", "LEADG", "DISHONG", "INTELG", "MORALB", "CARESB", "KNOWB", "LEADB", "DISHONB", "INTELB")

graphics.off()

i <- 3
eval(parse(text = paste0("p <- histogram( ~ ", var_n[i], " | cluster, data = dat_comp, layout = c(5,1))")))
plot(p)



# ---------------------------------------------------------------------------
# Visualize the clusters
#   - clusplot() will perform dimensional reduction with principal components or multidimensional scaling as the data warrant,
#     and then plot the observations with cluster membership identified.
# ---------------------------------------------------------------------------

library(cluster)


# lines = 0: omit distance lines between groups
clusplot(dat_comp, nes2_5$predclass, color = TRUE, shade = TRUE, labels = 5, lines = 0, main = "LCA plot (k = 5")



