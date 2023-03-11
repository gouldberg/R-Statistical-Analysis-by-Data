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
# Assess correlations between Euclidean distances in the tangent shape space with pair-wise Procrustes distance in the shape space
#
#  - Rohlf and Slice have shown that the generalized Procrustes superimposition was one of the less unbiased approaches
#    among the newly developed morphometric methods to estimate variation in the shape space.
#  - If variation is small, one can apply Euclidean statistics onto the tangent space coordinates.
# ------------------------------------------------------------------------------

# Full generalized Procrustes analysis
go <- fgpa(gorf.dat)



# ----------
# Extract Procrustes distance between configurations
( proc <- go$interproc.dist )


( proc.dist <- asin(proc) )



# ----------
# Compute Euclidean distancs in the tangent shape space
# correspond to the square root of the sum of the squared differences between the coordinates of projected aligned configurations.


# orp():  perform an orthogonal projection on a configuration dataset of the array class with p * k * n dimensions
( tango <- orp(go$rotated) )

n <- dim(gorf.dat)[3]

tang <- matrix(NA, n, n)


# ild2():  calculate the p interlandmark distances between two configurations
# tang[i,j]:  square root of sum of squared distance of p interlandmark distances between i and j configurations
ild2(tango[,,1], tango[,,2])

for(i in 1:n){
  for(j in 1:n){
    tang[i,j] <- sqrt(sum(ild2(tango[,,i], tango[,,j]) ^ 2))
  }
}


tang


( euc.dist <- as.dist(t(tang)) )



# ----------
plot(euc.dist, proc.dist, xlab = "euc.dist", ylab = expression(rho))
abline(0, 1, col = "grey50")


(cor(proc.dist, euc.dist))^2



# -->
# Here Procrustes and Euclidean distances are very well correlated, indicating that variation is small.



