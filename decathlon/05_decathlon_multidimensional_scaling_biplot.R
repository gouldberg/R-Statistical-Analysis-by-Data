setwd("//media//kswada//MyFiles//R//decathlon")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  decathlon
# ------------------------------------------------------------------------------

data("decathlon", package = "FactoMineR")

str(decathlon)

dim(decathlon)


decathlon



# ------------------------------------------------------------------------------
# Multidimensional Scaling Biplot
# ------------------------------------------------------------------------------

library(smacof)


fit_mds <- mds(decathlon[,1:10], type = "interval")


fit_mds

summary(fit_mds)



# ----------
extvar <- decathlon[,11:12]

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
# Highest R2vec:  Rank  --> this would be theoretical dimension .... but R2vec is small (0.21)
var <- "Rank"
calEm <- calibrate::calibrate(mdsbi$coef[,var], Y[,var], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE,
                              axiscol = "brown", axislab = var, labpos = 3, verb = TRUE)


# Lowest R2vec:  Points
var <- "Points"
calEm <- calibrate::calibrate(mdsbi$coef[,var], Y[,var], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE,
                              axiscol = "blue", axislab = var, labpos = 3, verb = TRUE)

