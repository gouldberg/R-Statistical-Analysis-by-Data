setwd("//media//kswada//MyFiles//R//gorf")

packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf and gorm
# ------------------------------------------------------------------------------

dim(gorf.dat)

dim(gorm.dat)



# ------------------------------------------------------------------------------
# Compute average form matrix
# ------------------------------------------------------------------------------

gorf.dat[,,1]



# replace some influential value to 1st landmark's y
gorf.dat_tmp <- gorf.dat

gorf.dat_tmp[1,2,1] <- 250

gorf.dat_tmp



# ----------
# Average form matrix
M <- mEDMA(gorf.dat_tmp)

M

round(M[row(M) > col(M)], 1)



# ------------------------------------------------------------------------------
# Compute variance-covariance form matrix
# ------------------------------------------------------------------------------

# Variance-covariance form matrix
vcvgm <- vEDMA(gorm.dat)

vcvgm



# ----------
# Check the diagonal elements and eigenvalues
ei <- eigen(vcvgm)

diag(vcvgm)
ei


# -->
# The variance-covariance matrix of EDMA usually has a rank equal to p - 1.
# However, diagonal elements may be negative when the variance of particular landmarks is very small, or
# when there are large covariances among some landmarks.
# Cole suggests remedying this problem by defining the "positive semi-defiinite matrix that is most similar to the estimate" of possible negative elements
# in the diagnal of vcv.



# ----------
round(ei$vectors %*% diag(ei$values) %*% t(ei$vectors), 1)


# In this respect, negative eigenvalues are set ot 0, and the matrix is re-estiamted.
# excluding the 3 last eigenvalue here.
p <- dim(gorm.dat)[1]
round(ei$vectors %*% diag(c(ei$values[1:(p-3)], rep(0,3))) %*% t(ei$vectors), 1)



