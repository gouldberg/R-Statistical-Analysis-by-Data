setwd("//media//kswada//MyFiles//R//golf")

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
# Obtain distance matrix from truss network
# ------------------------------------------------------------------------------

# see previous analysis
Ma

truss



# ----------
# Distance matrix obtained from truss network
Dista <- matrix(NA, 8, 8)

for(i in 1:nq){
  for(j in 1:4){
    for(k in 1:4){
      a <- truss[[i]][k]
      b <- truss[[i]][j]
      Dista[a, b] <- sqrt(sum((Ma[a,] - Ma[b,])^2))
    }
  }
}


Dista



# ------------------------------------------------------------------------------
# Reconstruct the relative positions of landmarks from the network distance
#   - Carpenter's approach:  estimate the locaiton of the series of landmarks starting from a prototype and weighting for known interlandmark distances
# ------------------------------------------------------------------------------

# We use the 2nd configuration as prototype shape, scaled dividing by 10 so that the algorithm works well
# even if the first estiamted distances are far from those in the final shape
( Xref <- gorf.dat[,,2] )
# ( Xap <- gorf.dat[,,2] )
( Xap <- gorf.dat[,,2] / 10 )


# ----------
DD <- Dista

a <- 1
b <- 10


# Carpenter suggest translating the coordinates of the centroid of successive configurations
# to the origin for insuring convergence, and to limit the number of iterations to 50

while(a < 50 & b > 0.01){

  X1 <- Xap
  
  # finding new locations for all landmarks and replacing prototype
  for(i in 1:8){
    # indices of missing interlandmark distances inlcluding the ith landmark and any other
    inc <- which(is.na(Dista[,i]))

    d1 <- dim(Xap[inc,])[1]
    d2 <- dim(Xap[inc,])[2]
    Dista[inc,i] <- sqrt(apply((Xap[inc,] - matrix(Xap[i,] , d1, d2, byrow = T)) ^ 2, 1, sum))
    Ra <- apply(Xap ^ 2, 1, sum)
    Y <- Dista[,i]^2 - Ra
    W <- diag(1,8)
    diag(W)[inc] <- 0
    
    coord <- solve((t(cbind(Xap, 1)) %*% W %*% cbind(Xap, 1))) %*% (t(cbind(Xap, 1)) %*% W %*% Y)
    Xap[i, 1] <- -coord[1] / 2
    Xap[i, 2] <- -coord[2] / 2
  }
  
  Xap[,1] <- Xap[,1] - mean(Xap[,1])
  Xap[,2] <- Xap[,2] - mean(Xap[,2])


  # b:  convergence check  
  b <- sum(abs(dist(Xap) - dist(X1)))
  Dista <- DD

  # count up
  a <- a + 1
}



# ------------------------------------------------------------------------------
# Draw the configurations 
# ------------------------------------------------------------------------------

par(mfrow=c(2,2))

# 3rd configuration original
plot(M, axes = F, xlab = "", ylab = "", main = "3rd original")
polygon(M)

# 2nd configuration original
plot(Xref, axes = F, xlab = "", ylab = "", main = "2nd original")
polygon(Xref)

# 3rd configuration oriented along longer axis
plot(Ma, axes = F, xlab = "", ylab = "", main = "3rd oriented along longer axis")
polygon(Ma)

# 2nd configuration reconstructed
plot(Xap, axes = F, xlab = "", ylab = "", main = "2nd reconstructed")
polygon(Xap)



