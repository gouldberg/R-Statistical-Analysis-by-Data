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
# Orient the configuration
# ------------------------------------------------------------------------------

# Orient the longer axis of the configuration parallel to the x-axis
# Multiplying the coordinates by the eigenvectors of the covariation matrix estimated from the coordinates
( Ma <- M %*% svd(var(M))$u )



# Compare the sign of the 1st angle of the original configuration with the one of the rotated configuration to detect for reflection
# angle2d():  calculates the angle between two 2D v1 and v2 vectors
if(round(angle2d(M[1,] - M[2,], M[3,] - M[2,]), 3) != round(angle2d(Ma[1,] - Ma[2,], Ma[3,] - Ma[2,]), 3))  Ma[,1] = -Ma[,1]

M
Ma



# ------------------------------------------------------------------------------
# Reconstruct the configuration following the approach of Carpenter et al.
# ------------------------------------------------------------------------------

# We need all landmarks to be involved in at least 3 interlandmark distances.
# If we have an odd number of landmarks, the landmark left over at an end is registered with one of the vertex of teh closest quadrilateral.

Ma1 <- Ma

truss <- list()

rownames(Ma1) <- 1:nrow(Ma1)

a <- 1

while(nrow(Ma1) > 4){
  truss[[a]] <- NA

  vert1 <- as.numeric(rownames(Ma1)[which.min(Ma1)])
  truss[[a]][1] <- vert1;  Ma1 <- Ma1[-which.min(Ma1),]

  vert1 <- as.numeric(rownames(Ma1)[which.min(Ma1)])
  truss[[a]][2] <- vert1;  Ma1 <- Ma1[-which.min(Ma1),]

  vert2 <- as.numeric(rownames(Ma1)[which.min(Ma1)])
  truss[[a]][3] <- vert2;  Ma2 <- Ma1[-which.min(Ma1),]
  
  vert2 <- as.numeric(rownames(Ma2)[which.min(Ma2)])
  truss[[a]][4] <- vert2;  Ma2 <- Ma1[-which.min(Ma2),]
  
  a <- a + 1
}


truss[[a]] <- as.numeric(rownames(Ma1))

if(length(truss[[a]]) == 3){
  truss[[a]] <- c(truss[[a]], truss[[a-1]][2])
}



# ----------
# each element of the list is obligatory in a set of indices for a quadrilateral.
# 1st quadrilateral vertices are chosen as the set of four landmarks, which coordinates are the moreo on the left.
# Further quadrilateral vertices are successively selected from the left to the right along the x-axis
truss

Ma



# ------------------------------------------------------------------------------
# Draw the truss network
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(Ma, asp = 1, axes = F, xlab = "", ylab = "")

nq <- length(truss)

for(i in 1:nq){
  for(j in 1:length(truss[[i]])){
    for(k in 1:j){
      segments(Ma[truss[[i]][k],1], Ma[truss[[i]][k], 2], Ma[truss[[i]][j],1], Ma[truss[[i]][j],2])
    }
  }
}


