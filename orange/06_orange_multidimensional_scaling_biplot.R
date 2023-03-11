# setwd("//media//kswada//MyFiles//R//orange")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\orange")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orange
# ------------------------------------------------------------------------------

orange <- read.table("orange.csv", header=TRUE, sep=";", dec=".", row.names=1)

str(orange)

dim(orange)


orange




# ------------------------------------------------------------------------------
# Multidimensional Scaling Biplot
# ------------------------------------------------------------------------------

library(smacof)


dat_mat <- psych::cor2dist(cor(t(orange[,1:7])))


fit_mds <- mds(dat_mat, type = "interval")



fit_mds



# -->
# stress-1 value is only 0.012



summary(fit_mds)




# ----------
plot(fit_mds)




# ----------
extvar <- orange[,8:14]


# biplotmds function standardized these external variables and return the standardized regression coefficients
# (as well as the R^2 vector)

mdsbi <- biplotmds(fit_mds, extvar = extvar)


plot(mdsbi)



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
var <- "Vitamin.C"
calEm <- calibrate::calibrate(mdsbi$coef[,var], Y[,var], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE,
                              axiscol = "brown", axislab = var, labpos = 3, verb = TRUE)


# Lowest R2vec:  Fructose
var <- "Fructose"
calEm <- calibrate::calibrate(mdsbi$coef[,var], Y[,var], tm = seq(-2, 2, by = 0.5), Fr = X, dp = TRUE,
                              axiscol = "blue", axislab = var, labpos = 3, verb = TRUE)


