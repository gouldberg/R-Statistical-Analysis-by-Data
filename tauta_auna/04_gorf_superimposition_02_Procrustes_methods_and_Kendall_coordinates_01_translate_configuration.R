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
# Removing location:  Translate configuration so that the centroid is sent at the origin
# ------------------------------------------------------------------------------

test <- gorf.dat[,,5]



# ----------
# translated configuration so that the centroid is sent at the origin
test_transl <- transl(test)

test
test_transl


apply(test, 2, sum)
apply(test_transl, 2, sum)


centsiz(test)
centsiz(test_transl)



# ----------
graphics.off()

par(mfrow=c(1,2))
plot(test, axes = F, xlab = "", ylab = "")
polygon(test)

plot(test_transl, axes = F, xlab = "", ylab = "")
polygon(test_transl)




# ------------------------------------------------------------------------------
# Removing location:  Translate configuration so that the centroid is sent at the origin
#  - multiplying centering matrix
# ------------------------------------------------------------------------------

# Alternatively one can obtain the centered configuration (noted X) from original configurations (M)
# by premultiplying the configuration matrix (M) by the centering matrix
# The centering matrix has a diagonalequal to 1 - 1/p and lower and upper triangle cells equal to - 1/p

p <- nrow(test)
cm1 <- diag(1, p) - 1/p
cm1 %*% test

test_transl




# ------------------------------------------------------------------------------
# Removing location:  Translate configuration so that the centroid is sent at the origin
#  - multiplying centering matrix based on Helmert matrix
# ------------------------------------------------------------------------------

helmert(p)



# ----------
# multiplication of the sub-Helmert matrix by its transpose is equal to the centering matrix
t(helmert(p)[-1,]) %*% helmert(p)[-1,]

diag(1, p) - 1/p



# ----------
t(helmert(p)[-1,]) %*% helmert(p)[-1,] %*% test

test_transl

