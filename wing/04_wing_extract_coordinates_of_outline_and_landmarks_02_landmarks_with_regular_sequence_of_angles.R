setwd("//media//kswada//MyFiles//R//wing")

packages <- c("dplyr", "pixmap")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

source(file = "//media//kswada//MyFiles//R_basics//functions_for_morphometrics.R")



# ------------------------------------------------------------------------------
# data:  wing
# ------------------------------------------------------------------------------

M <- read.pnm("//media//kswada//MyFiles//references//MorphometricsWithR//e-supplement//wing.ppm")

M


str(M)


# -----------
plot(M)



# ------------------------------------------------------------------------------
# Conte:  Extract the coordinates of outline pixels
# ------------------------------------------------------------------------------

# We use the revised version
Conte <- function(x, imagematrix){
  I <- imagematrix
  x <- rev(x)
  x[1] <- dim(I)[1] - x[1]
  
  while(abs(I[x[1], x[2]] - I[x[1], (x[2] - 1)]) < 0.1){
    x[2] <- x[2] - 1
  }
  
  # x[2] <- x[2] - 1
  
  a <- 1
  
  M <- matrix(c(0, -1, -1, -1, 0, 1, 1, 1, 1, 1, 0, -1, -1, -1, 0, 1), 2, 8, byrow = T)
  M <- cbind(M[,8], M, M[,1])
  
  X <- 0;  Y<- 0;
  x1 <- x[1];  x2 <- x[2];
  SS <- NA;  S <- 6;
  
  while( (any(c(X[a], Y[a]) != c(x1, x2)) | length(X) < 3) ){
  # while( a < 2000 ){
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
# Binarize image by insuring taht full rows and columns of white pixels border
# ------------------------------------------------------------------------------
y <- as(M, "pixmapGrey")

str(y)


# ----------
# IMPORTANT !!!:  For insuring that full rows and columns of white pixels border,
# we append one row of white pixels around the binarized image.
y@grey <- cbind(1, y@grey, 1)
y@grey <- rbind(1, y@grey, 1)
y@size <- as.integer(c(371+2, 904+2))
y@bbox <- c(0, 0, 904+2, 371+2)


str(y)


layout(matrix(1))

thresh <- 0.95

y@grey[which(y@grey >= thresh)] <- 1

y@grey[which(y@grey < thresh)] <- 0

par(mar = c(1,1,1,1))

plot(y)



# ------------------------------------------------------------------------------
# Extract the coordinates of outline pixels by Conte()
# ------------------------------------------------------------------------------

# locating exactly a point on the outline
start <- round(unlist(locator(1)), 0)

Rc <- Conte(start, y@grey)

lines(Rc$X, Rc$Y, lwd = 4, col = "blue")

Rc


length(Rc$X)
length(Rc$Y)



# ------------------------------------------------------------------------------
# Scale the data to retrieve raw coordinates, using the ruler on the bottom of the image
# ------------------------------------------------------------------------------

# We can click on the two extreme graduations with locator to estimate the number of the pixels contained on 5mm

scalecoord <- locator(2, type = "p", pch = 4, lwd = 2)

scalepixsize <- sqrt(diff(scalecoord$x) ^ 2 + diff(scalecoord$y) ^ 2)

scalecoord

scalepixsize



X <- Rc$X * 5 / scalepixsize

Y <- Rc$Y * 5 / scalepixsize

Xc <- mean(X)

Yc <- mean(Y)



# ------------------------------------------------------------------------------
# Extract landmarks that are spaced with a regular sequence of angles taken between the outline coordinates and the centroid
# ------------------------------------------------------------------------------

plot(X, Y, type = "l", lwd = 1.5, asp = 1, axes = F, main = "polar")

points(Xc, Yc, pch = 4)



# ----------
k <- 64

ju <- regularradius(X, Y, k)

points(ju$coord[,1] + Xc, ju$coord[,2] + Yc)

for(i in 1:k){
  segments(0 + Xc, 0 + Yc, ju$coord[,1] + Xc, ju$coord[,2] + Yc)
}

