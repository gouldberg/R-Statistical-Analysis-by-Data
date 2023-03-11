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
  
  # while( (any(c(X[a], Y[a]) != c(x1, x2)) | length(X) < 3) ){
  while( a < 1000 ){
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


# IT TAKES TIME !!!!
Rc <- Conte(c(round(start$x), round(start$y)), y@grey)

lines(Rc$X, Rc$Y, lwd = 4)

arrows(0, Rc$Y[1], Rc$X[1], Rc$Y[1], length = 0.1)



# ------------------------------------------------------------------------------
# Outline smoothing
#  - Haines and Crampton recommend smoothing the outline based on the following formula (see funciton smoothout())
# ------------------------------------------------------------------------------

smoothout <- function(M, n){
  
  p <- dim(M)[1]
  
  a <- 0
  
  while(a <= n){
    a <- a + 1
    
    Ms <- rbind(M[p,], M[-p,])
    Mi <- rbind(M[-1,], M[1,])
    M <- M/2 + Ms/4 + Mi/4
  }
  return(M)
}



# ----------
Rc_mat <- as.matrix(data.frame(Rc))


# n: number of iterations to apply smoothing function
n <- 400

Rc_smooth <- smoothout(M = Rc_mat, n = n)


plot(Rc$X, Rc$Y, type = "l", lwd = 1.5, asp = 1, axes = F)

par(new=T)
plot(Rc_smooth[,1], Rc_smooth[,2], type = "l", lwd = 1.5, lty = 2, asp = 1, axes = F, col = "gray")

