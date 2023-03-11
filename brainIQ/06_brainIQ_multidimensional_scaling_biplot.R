setwd("//media//kswada//MyFiles//R//brainIQ")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brainIQ
# ------------------------------------------------------------------------------

data("BrainIQ", package = "MPsychoR")

str(BrainIQ)



# ----------
# omit NAs and gender
# BrainIQ <- na.omit(BrainIQ[, -1])



# ------------------------------------------------------------------------------
# Multidimensional Scaling Biplot
# ------------------------------------------------------------------------------

library(smacof)


fit_mds <- mds(BrainIQ[,c(-1,-5,-6,-7)], type = "interval")


fit_mds

summary(fit_mds)



# ----------
extvar <- BrainIQ[,c(5,6,7)]

# biplotmds function standardized these external variables and return the standardized regression coefficients
# (as well as the R^2 vector)
mdsbi <- biplotmds(fit_mds, extvar = extvar)


mdsbi$coef

mdsbi$R2vec



# ----------
( X <- fit_mds$conf )

Y <- scale(extvar, scale = TRUE)



# ----------
plot(X, pch = 20, cex = 0.6, xlab = "Dimension 1", ylab = "Dimension 2", col = "darkblue", asp = 1, main = "Biplot MDS Axis")

text(X, labels = rownames(X), cex = 0.7, pos = 3, col = "darkblue")

abline(h = 0, v = 0, lty = 2, col = "gray")



# ----------
# Highest R2vec:  Vitamin.C  --> this would be theoretical dimension
var <- "Height"
calEm <- calibrate::calibrate(mdsbi$coef[,var], Y[,var], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE,
                              axiscol = "brown", axislab = var, labpos = 3, verb = TRUE)


# Lowest R2vec:  Fructose
var <- "Weight"
calEm <- calibrate::calibrate(mdsbi$coef[,var], Y[,var], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE,
                              axiscol = "blue", axislab = var, labpos = 3, verb = TRUE)

