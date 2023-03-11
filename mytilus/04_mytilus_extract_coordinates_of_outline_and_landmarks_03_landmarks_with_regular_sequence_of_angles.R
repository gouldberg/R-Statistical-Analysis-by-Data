setwd("//media//kswada//MyFiles//R//mytilus")

packages <- c("dplyr", "pixmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mytilus
# ------------------------------------------------------------------------------

M <- read.pnm("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//mytilus.ppm")

M


str(M)


# -----------
plot(M)



# ------------------------------------------------------------------------------
# Conte:  Extract the coordinates of outline pixels
# ------------------------------------------------------------------------------


Conte <- function(x, imagematrix){
  I <- imagematrix
  x <- rev(x)
  x[1] <- dim(I)[1] - x[1]
  
  # while(abs(I[x[1], x[2]] - I[x[1], (x[2] - 1)]) < 0.1){
  #  x[2] <- x[2] - 1
  #}
  
  x[2] <- x[2] - 1

  a <- 1
  
  M <- matrix(c(0, -1, -1, -1, 0, 1, 1, 1, 1, 1, 0, -1, -1, -1, 0, 1), 2, 8, byrow = T)
  M <- cbind(M[,8], M, M[,1])
  
  X <- 0;  Y<- 0;
  x1 <- x[1];  x2 <- x[2];
  SS <- NA;  S <- 6;
  
  while( (any(c(X[a], Y[a]) != c(x1, x2)) | length(X) < 3) ){
  # while( a < 1000 ){
      if(abs(I[x[1] + M[1, S+1], x[2] + M[2, S+1]] - I[x[1], x[2]]) < 0.1){
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[,S+1]
      SS[a] <- S + 1
      S <- (S + 7) %% 8
    } else if(abs(I[x[1] + M[1, S+2], x[2] + M[2, S+2]] - I[x[1], x[2]]) < 0.1){
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[,S+2]
      SS[a] <- S + 2
      S <- (S + 7) %% 8
    } else if(abs(I[x[1] + M[1, S+3], x[2] + M[2, S+3]] - I[x[1], x[2]]) < 0.1){
      a <- a + 1
      X[a] <- x[1]
      Y[a] <- x[2]
      x <- x + M[,S+3]
      SS[a] <- S + 3
      S <- (S + 7) %% 8
    } else {
      S <- (S + 1) %% 8
    }
  }

  list(X = (Y[-1]), Y = ((dim(I)[1] - X))[-1])
}



# ------------------------------------------------------------------------------
# Extract the coordinates of outline pixels by Conte()
# ------------------------------------------------------------------------------
layout(matrix(1))

y <- as(M, "pixmapGrey")

y@grey[which(y@grey >= 0.9)] <- 1

y@grey[which(y@grey < 0.9)] <- 0.7

par(mar = c(1,1,1,1))

plot(y)



# ----------
# locating exactly a point on the outline
start <- locator(1)

Rc <- Conte(c(round(start$x), round(start$y)), y@grey)

lines(Rc$X, Rc$Y, lwd = 4)

arrows(0, Rc$Y[1], Rc$X[1], Rc$Y[1], length = 0.1)



# ------------------------------------------------------------------------------
# Extract landmarks that are spaced with a regular sequence of angles taken between the outline coordinates and the centroid
# ------------------------------------------------------------------------------

regularradius <- function(Rx, Ry, n){

  le <- length(Rx)
  
  M <- matrix(c(Rx, Ry), le, 2)
  
  M1 <- matrix(c(Rx - mean(Rx), Ry - mean(Ry)), le, 2)
  
  V1 <- complex(real = M1[,1], imaginary = M1[,2])
  
  # Arg() is an angle
  M2 <- matrix(c(Arg(V1), Mod(V1)), le, 2)
  
  V2 <- NA

  for(i in 0:(n - 1)){
    V2[i + 1] <- which.max((cos(M2[,1] - 2 * i * pi / n)))
  }
  
  V2 <- sort(V2)
  
  list("pixindices" = V2, "radii" = M2[V2, 2], "coord" = M1[V2,])
}



# ----------
Xc <- mean(Rc$X)

Yc <- mean(Rc$Y)

plot(Rc$X, Rc$Y, type = "l", lwd = 1.5, asp = 1, axes = F, main = "polar")

points(Xc, Yc, pch = 4)



# ----------
k <- 32

ju <- regularradius(Rc$X, Rc$Y, k)

points(ju$coord[,1] + Xc, ju$coord[,2] + Yc)

for(i in 1:k){
  segments(0 + Xc, 0 + Yc, ju$coord[,1] + Xc, ju$coord[,2] + Yc)
}
