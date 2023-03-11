setwd("//media//kswada//MyFiles//R//gorf")


packages <- c("dplyr", "shapes")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  gorf
# ------------------------------------------------------------------------------

shapes::gorf.dat

str(gorf.dat)


( M <- gorf.dat[,,3] )



# ----------
# modify the data
# M[6,2] <- -M[6,2]



# ------------------------------------------------------------------------------
# Plot configuration and its centroid
# ------------------------------------------------------------------------------

centcoord(M)



# ----------
plot(M, axes = F, xlab = "", ylab = "")
polygon(M)
points(t(centcoord(M)), col = "blue")




# ------------------------------------------------------------------------------
# Scaling the configuration to unit centroid size
# ------------------------------------------------------------------------------

( Ms <- centsiz(M) )


# ----------
plot(Ms$scaled, axes = F, xlab = "", ylab = "")
polygon(Ms$scaled)
points(t(centcoord(Ms$scaled)), col = "blue")



# ------------------------------------------------------------------------------
# Baseline size and scaled configuration
#   - baseline size is the distance between two landmarks and proposed as a measure of the size scalar
# ------------------------------------------------------------------------------


# number of landmarks
n <- 8


b_size <- matrix(NA, n, n)

for(i in 1:n){
  for(j in 1:i){
    b_size[i,j] <- basesiz(M, i, j)
  }
}


b_size



# it is the same....
dist(M)



