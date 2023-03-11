setwd("//media//kswada//MyFiles//R//gorf")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf, panf, pongof
# ------------------------------------------------------------------------------
data(gorf.dat, package = "shapes")
data(panf.dat, package = "shapes")
data(pongof.dat, package = "shapes")

dim(gorf.dat)

dim(panf.dat)

dim(pongof.dat)



# ------------------------------------------------------------------------------
# Check configurations by sample
# ------------------------------------------------------------------------------

idx_gf <- sample(dim(gorf.dat)[3], size = 2, replace = FALSE)
idx_pf <- sample(dim(panf.dat)[3], size = 2, replace = FALSE)
idx_pgf <- sample(dim(pongof.dat)[3], size = 2, replace = FALSE)


par(mfrow=c(2,3), mar=c(2,2,2,2))
plot(gorf.dat[,,idx_gf[1]], axes = FALSE, xlab = "", ylab = "", main = paste("gorf", idx_gf[1]))
polygon(gorf.dat[,,idx_gf[1]])

plot(gorf.dat[,,idx_gf[2]], axes = FALSE, xlab = "", ylab = "", main = paste("gorf", idx_gf[2]))
polygon(gorf.dat[,,idx_gf[2]])

plot(panf.dat[,,idx_pf[1]], axes = FALSE, xlab = "", ylab = "", main = paste("panf", idx_pf[1]))
polygon(panf.dat[,,idx_pf[1]])

plot(panf.dat[,,idx_pf[2]], axes = FALSE, xlab = "", ylab = "", main = paste("panf", idx_pf[2]))
polygon(panf.dat[,,idx_pf[2]])

plot(pongof.dat[,,idx_pgf[1]], axes = FALSE, xlab = "", ylab = "", main = paste("pongf", idx_pgf[1]))
polygon(pongof.dat[,,idx_pgf[1]])

plot(pongof.dat[,,idx_pgf[2]], axes = FALSE, xlab = "", ylab = "", main = paste("pongf", idx_pgf[2]))
polygon(pongof.dat[,,idx_pgf[2]])



# ----------
# dataset must be first reorganized because landmarks are not labeled in the same order for each set.

gorf <- gorf.dat
panf <- panf.dat[c(5,1,2:4,6:8),,]
pongof <- pongof.dat[c(5,1,2:4,6:8),,]

# gorf <- gorf.dat[c(5,1,2:4,6:8),,]
# panf <- panf.dat
# pongof <- pongof.dat

APE <- array(c(panf, gorf, pongof), dim = c(8,2,80))



# ------------------------------------------------------------------------------
# Superimposed by aligne() --> pgpa() --> orp()
# ------------------------------------------------------------------------------

AP <- orp(pgpa(aligne(APE))$rotated)

AP



# ----------
idx <- sample(dim(APE)[3], size = 3, replace = FALSE)

graphics.off()
par(mfrow=c(2,3), mar=c(2,2,2,2))
plot(APE[,,idx[1]], axes = FALSE, xlab = "", ylab = "", main = paste("original", idx[1]))
polygon(APE[,,idx[1]])

plot(APE[,,idx[2]], axes = FALSE, xlab = "", ylab = "", main = paste("original", idx[2]))
polygon(APE[,,idx[2]])

plot(APE[,,idx[3]], axes = FALSE, xlab = "", ylab = "", main = paste("original", idx[3]))
polygon(APE[,,idx[3]])

plot(AP[,,idx[1]], axes = FALSE, xlab = "", ylab = "", main = paste("superimposed", idx[1]))
polygon(AP[,,idx[1]])

plot(AP[,,idx[2]], axes = FALSE, xlab = "", ylab = "", main = paste("superimposed", idx[2]))
polygon(AP[,,idx[2]])

plot(AP[,,idx[3]], axes = FALSE, xlab = "", ylab = "", main = paste("superimposed", idx[3]))
polygon(AP[,,idx[3]])



# ------------------------------------------------------------------------------
# Test for differences between within-group and between-group variance-covariance marices with the adapted Hotelling-Lawley multivariate test
# ------------------------------------------------------------------------------

fact <- as.factor(c(rep("p", 26), rep("g", 30), rep("o", 24)))

m <- t(matrix(AP, 16, 80))

n <- dim(m)[1]



# ----------
# linear model, explaining factor being species
mod1 <- lm(m ~ as.factor(fact))


summary(mod1)


dfef <- length(levels(fact)) - 1

dfer <- n - length(levels(fact))

SSef <- (n - 1) * var(mod1$fitted.values)

SSer <- (n - 1) * var(mod1$residuals)



# ----------
Hotellingsp(SSef, SSer, dfef, dfer)



# -->
# The group means are significantly different


