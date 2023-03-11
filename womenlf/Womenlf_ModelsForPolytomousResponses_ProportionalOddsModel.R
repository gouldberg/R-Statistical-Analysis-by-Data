# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
packages <- c("dplyr", "vcd", "vcdExtra", "lmtest", "ggplot2", "directlabels", "MASS", "effects", "car", "VGAM", "rms")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Womenlf
#  - The resulf of a 1977 Canadian survey, containing data for 263 married women of age 21-30 who indicated their working status (outside the home)
#    as not working, working part time, or working full time, together with their husband's income and a binary indicator of whether they had one or more young
#    children in their household.
# ------------------------------------------------------------------------------
data("Womenlf", package = "car")

dim(Womenlf)
str(Womenlf)


car::some(Womenlf)


data <- Womenlf
data$partic <- ordered(data$partic, levels = c("not.work", "parttime", "fulltime"))
levels(data$partic)



# ------------------------------------------------------------------------------
# Fitting the proportional odds model
# ------------------------------------------------------------------------------
# Hess = TRUE to have the function return the observed information matrix (Hessian), that is used in other operations to calculate standard errors.
# summary() does not provide no significant tests or p-values
wlf.polr <- MASS::polr(partic ~ hincome + children, data = data, Hess = TRUE)
summary(wlf.polr)



# ----------
# cat::Anova() method gives the appropriate tests
car::Anova(wlf.polr)



# ------------------------------------------------------------------------------
# Testing the proportional odds assumption
# ------------------------------------------------------------------------------
# parallel = TRUE, this is equivalent to the polr() model, except that the signs of the coefficients are reserved.
wlf.po <- VGAM::vglm(partic ~ hincome + children, data = data, family = cumulative(parallel = TRUE))
wlf.po


# The generalized logit model
data$partic <- relevel(data$partic, ref = "not.work")
levels(data$partic)
wlf.multinom <- nnet::multinom(partic ~ hincome + children, data = data, Hess = TRUE)
wlf.multinom



# ----------
# The LR test can be calculated manually using the difference in residual deviance for the two models.
tab <- cbind(
  Deviance = c(deviance(wlf.multinom), deviance(wlf.po)),
  df = c(df.residual(wlf.multinom), df.residual(wlf.po))
)
tab <- rbind(tab, diff(tab))
rownames(tab) <- c("GenLogit", "PropOdds", "LR test")
tab <- cbind(tab, pvalue=1-pchisq(tab[,1], tab[,2]))
tab



# ------------------------------------------------------------------------------
# Graphical asessment of proportional odds
#  - plot of conditional X means
# ------------------------------------------------------------------------------
wlf.po2 <- rms::lrm(partic ~ hincome + children, data = data)
wlf.po2



# ----------
# Plot of conditional X means, producing one marginal panel for each predictor in the model.
# For categorical predictors, it plots only the overall most frequent category
# Solid lines connect the stratified means of X given Y.
# Dashed lines show the estimated expected value of X given Y = j if the proportional odds model holds for X
op <- par(mfrow=c(1,2))
rms::plot.xmean.ordinaly(partic ~ hincome + children, data=data, lwd=2, pch=16, subn=TRUE, cr = TRUE)
par(op)


# --> Proportional odds model does not apply


